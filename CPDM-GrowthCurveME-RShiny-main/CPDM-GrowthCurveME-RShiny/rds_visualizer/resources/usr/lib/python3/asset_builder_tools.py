#!/usr/bin/python3
import argparse
import os
import re
import sys
import subprocess
import tempfile
import json
from datetime import datetime
import random
import time

import dxpy
import re

from dxpy.utils.resolver import *
import dx_utils
import logging
from dxpy.dxlog import DXLogHandler

logger = logging.getLogger(__name__)
logger.setLevel(logging.WARNING)
logger.propagate = False

console_log_handler = logging.StreamHandler()
console_log_handler.setFormatter(logging.Formatter("%(levelname)s: %(message)s"))
logger.addHandler(console_log_handler)
try:
    logger.addHandler(DXLogHandler())
except:
    logger.error("Job logging is not available.")


DX_HOME_DIR = "/home/dnanexus/"
# specify paths that should be ignored when creating snapshot
# job specific files:
DX_JOB_ID = dxpy.JOB_ID or os.environ.get("DX_JOB_ID")
DX_JOB_FILES = ["{}".format(DX_JOB_ID),
                "{}.code.py".format(DX_JOB_ID),
                "job_error_reserved_space",
                "job_input.json",
                "job_error.json"]
DX_STD_LOG_FILES = ["/dx_std*", DX_HOME_DIR + "dx_std*"]  # dx_stdout dx_stderr
DX_TIMEOUT_FILE = [".dx.timeout"]
DX_SSH_DIR = [".ssh*"]
DX_JOB_DESC_FILES = ["dnanexus-*"]  # exec and job json
DX_JOB_IGNORE_PATH = DX_JOB_FILES + DX_SSH_DIR + \
    DX_TIMEOUT_FILE + DX_JOB_DESC_FILES
# env files
DX_ENV_IGNORE_PATH = [".byobu*", ".cache*", "environment"]
# dx login conf files:
# "DX_USER_CONF_DIR" or "~/.dnanexus_config"
DX_CONFIG_PATH = [dxpy.config.get_user_conf_dir() + "*"]

DEFAULT_IGNORE_PATH = DX_CONFIG_PATH + \
    DX_STD_LOG_FILES + \
    [DX_HOME_DIR + path for path in DX_JOB_IGNORE_PATH] + \
    [DX_HOME_DIR + path for path in DX_ENV_IGNORE_PATH]

# extra system files to be excluded
FILES_TO_IGNORE = {'/var/cache/ldconfig',
                   '/var/lib/dpkg/lock', '/var/lib/dpkg/triggers/Lock'}

# system file lists used to create snapshot
BEFORE_FILENAME = 'before-sorted.txt'
AFTER_FILENAME = 'after-sorted.txt'

###########
# Code from create_asset_focal
# https://github.com/dnanexus/dx_app_builder/blob/16cca82af6fe41afb4a74828fafbc3992de359c7/asset_builder_py3.py
###########


class DXPopen(subprocess.Popen):
    def __init__(self, *popenargs, **kwargs) -> None:
        self.env = os.environ.copy()
        self.env['LC_ALL'] = 'C'
        kwargs['env'] = self.env
        super().__init__(*popenargs, **kwargs)


def create_before_file_list():
    before_file_list_path = os.path.join(
        tempfile.gettempdir(), BEFORE_FILENAME)
    get_system_snapshot(before_file_list_path)


def get_file_list(output_file, resources_to_ignore=DEFAULT_IGNORE_PATH):
    """
    This method find all the files in the system and writes it to the output file
    """
    tmp_dir = os.path.dirname(output_file) + "*"
    skipped_paths = ["/proc*",
                     tmp_dir,
                     "/run*",
                     "/boot*",
                     "/sys*",
                     "/dev*",
                     "/tmp*",
                     "/var/cache*",
                     "/var/tmp*",
                     "/var/log*",
                     "/var/lib/lxc*",
                     "/var/lib/tmp*",
                     "/var/lib/pcp*",
                     "/var/lib/systemd*",
                     "/root*",
                     "/var/run*",
                     "/etc/mtab"]
    if get_dist_and_release() == ("Ubuntu", "20.04"):
        # Ubuntu 20.04 /bin and /sbin are symlinks to /usr/bin and /usr/sbin
        skipped_paths.extend(["/bin", "/sbin", "/snap/*"])
    cmd = ["sudo", "find", "/"]
    ignore_cmd = []
    for ignore_dir in skipped_paths + resources_to_ignore:
        ignore_cmd.extend(["-path", ignore_dir, "-prune", "-o"])
    if ignore_cmd:
        cmd.extend(ignore_cmd + ["-print"])
    ps_pipe = DXPopen(cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    ps_file = DXPopen(["sort"], stdin=ps_pipe.stdout, stdout=subprocess.PIPE)

    with open(output_file, "w") as bfile:
        for line in ps_file.stdout:
            sp_code = ps_file.poll()
            file_name = line.decode().rstrip()
            if file_name == "":
                if sp_code is not None:
                    break
                else:
                    continue
            if file_name == "/":
                continue
            try:
                mtime = str(os.path.getmtime(file_name))
            except OSError as os_err:
                #print(os_err)
                mtime = ''
            # file_name should not have special characters
            bfile.write(str(file_name) + "\t" + str(mtime) + "\n")
    ps_file.stdout.close()


def get_system_snapshot(output_file_path, ignore_files=DEFAULT_IGNORE_PATH):
    tmp_file_name = "file_" + \
        str(random.randint(0, 1000000)) + "_" + \
        str(int(time.time() * 1000)) + ".txt"
    tmp_file_path = os.path.join(tempfile.gettempdir(), tmp_file_name)
    get_file_list(tmp_file_path, ignore_files)
    with open(output_file_path, 'w') as output_file_handle:
        proc = DXPopen(['sort', tmp_file_path], stdout=output_file_handle)
        proc.communicate()
    os.chmod(output_file_path, mode=0o777)

def get_file_diffs(first_file, second_file, diff_file):
    """ Get difference between two txt files and write the difference to the
    third file.
    """
    cmd = ["sudo", "comm", "--check-order", "-13", first_file, second_file]
    ps_pipe = DXPopen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = ps_pipe.communicate()
    if ps_pipe.returncode != 0:
        err_msg = err.decode().rstrip()
        raise Exception(err_msg)
    
    with open(diff_file, "w") as bfile:
        for line in out.decode().splitlines():
            file_name = '\t'.join(line.rstrip().split('\t')[:-1])
            if len(file_name) > 0 and file_name not in FILES_TO_IGNORE:
                bfile.write(file_name + '\n')
                print(file_name)


def get_parsed_args():
    parser = argparse.ArgumentParser(prog="dx-create-snapshot",
                                     description="Save the file system changes in this cloud_workstation as a snapshot file",
                                     add_help=True)
    parser.add_argument(
        'task', choices=['asset', 'snapshot', 'check_version'], help=argparse.SUPPRESS)
    parser.add_argument(
        "--name",
        required=False,
        default=None,
        help="Name of snapshot")
    parser.add_argument(
        "--title",
        required=False,
        help=argparse.SUPPRESS)
    parser.add_argument(
        "--description",
        required=False,
        help=argparse.SUPPRESS)
    parser.add_argument(
        "--version",
        required=False,
        help=argparse.SUPPRESS)
    parser.add_argument(
        "--snapshot_input",
        required=False,
        help=argparse.SUPPRESS)
    try:
        args, unknown_args = parser.parse_known_args()
    except:
        sys.exit(1)
    else:
        if unknown_args:
            print("Argument {} is not accepted.\n".format("/".join(unknown_args)))
            parser.print_help()
            sys.exit(1)
        return args


def get_dist_and_release():
    try:
        import lsb_release
        distribution = lsb_release.get_os_release().get("ID", "Ubuntu")
        release      = lsb_release.get_os_release().get("RELEASE", "20.04")
    except:
        try:
            cmd = 'lsb_release -a '
            distribution = str(subprocess.check_output(cmd + "| grep Distributor | awk '{print $3}'", shell=True).strip().decode())
            release      = str(subprocess.check_output(cmd + "| grep Release | awk '{print $2}'", shell=True).strip().decode())
        except:
            distribution, release = "Ubuntu", "20.04"
    
    return (distribution, release)


def create_snapshot(name, ignore_files, visibility='visible'):
    if name is None:
        name = "{}.snapshot".format(datetime.now().strftime('%B_%d_%Y_%H_%M'))

    before_file_list_path = os.path.join(
        tempfile.gettempdir(), BEFORE_FILENAME)
    after_file_list_path = os.path.join(tempfile.gettempdir(), AFTER_FILENAME)
    get_system_snapshot(after_file_list_path, ignore_files)

    diff_file_path = os.path.join(tempfile.gettempdir(), "diff.txt")
    get_file_diffs(before_file_list_path, after_file_list_path, diff_file_path)

    tar_name = re.sub(r"\s+", '-', name)
    tar_output = dxpy.PROJECT_CONTEXT_ID + ":" + tar_name
    exe_desc = get_running_exec_desc()
    tar_details = {'executable': exe_desc.get("id"),
                   'executable_name': exe_desc.get('name'),
                   "executable_version": exe_desc.get('version')}
    tar_cmd = ["sudo", "tar", "-Pcz", "--no-recursion", "-T", diff_file_path, "-f", "-"]
    tar_ps = DXPopen(tar_cmd, stdout=subprocess.PIPE)
    upload_ps = DXPopen(["dx", "upload", "-", "--wait", "--brief", "-o", tar_output,
                          "--details", json.dumps(tar_details), "--visibility", visibility],
                         stdin=tar_ps.stdout, stdout=subprocess.PIPE)

    tar_ps.stdout.close()
    snapshot_tarball_id = str(upload_ps.communicate()[0].decode()).rstrip()
    return snapshot_tarball_id, tar_output


def create_asset(name, title, description, version):
    if name is None:
        name = "{}.tar.gz".format(datetime.now().strftime('%B_%d_%Y_%H_%M'))
    asset_tarball_id, _ = create_snapshot(
        name, ["/home/dnanexus*"], visibility='hidden')

    record_name = name
    record_details = {"archiveFileId": {"$dnanexus_link": asset_tarball_id}}
    record_properties = {"version": version,
                         "title": title,
                         "description": description}
    asset_bundle = dxpy.new_dxrecord(name=record_name,
                                     types=["AssetBundle"], details=record_details,
                                     properties=record_properties, close=True,
                                     project=dxpy.PROJECT_CONTEXT_ID)

    with open('dxasset.json', 'w') as fh:
        distribution, release = get_dist_and_release()
        fh.write(json.dumps({'name': name, 'title': title, 'description': description,
                 'version': version, 'distribution': distribution, 'release': release}))

    return asset_bundle.get_id(), asset_bundle.get_proj_id() + ":" + record_name


def get_running_exec_desc():
    """
    Get the running executable description.
    These info are stored in /home/dnanexus/dnanexus-executable.json. 
    If the file does not exist, these can be retrieved by describing the running job (whose id is found in /home/dnanexus/dnanexus-job.json, dxpy.JOB_ID, or DX_JOB_ID in environment)

    :return: description of the current executable
    :rtype: dict
    """
    exec_desc = {}
    DX_EXEC_JSON = DX_HOME_DIR + "dnanexus-executable.json"
    DX_JOB_JSON = DX_HOME_DIR + "dnanexus-job.json"

    if os.path.exists(DX_EXEC_JSON):
        with open(DX_EXEC_JSON) as exec_json:
            exec_desc = json.load(exec_json)

    else:
        if os.path.exists(DX_JOB_JSON):
            with open(DX_JOB_JSON) as job_json:
                job_details = json.load(job_json)
            exec_desc = dxpy.describe(job_details.get("executable"))

        elif DX_JOB_ID:
            job_details = dxpy.describe(DX_JOB_ID)
            exec_desc = dxpy.describe(job_details.get("executable"))

    return exec_desc

def get_snapshot_exec_desc(snapshot_input: str):
    """
    Get the description of the executable that generated the target snapshot file

    :param snapshot_input: file ID or path to the target snapshot
    :type snapshot_input: str
    :return: description of the snapshot executable. 
    :rtype: dict
    """
    _, _, snapshot_result = resolve_existing_path(snapshot_input,
                                                  expected_classes=["file"],
                                                  describe={"fields": {"details": True, "createdBy": True}})
    if snapshot_result is None:
        raise ResolutionError("Could not resolve {} to a snapshot file.".format(snapshot_input))

    snapshot_desc = snapshot_result["describe"]

    # initialize snapshot exec description from snapshot details
    snapshot_exec_desc= {
        "id": snapshot_desc["details"].get("executable"),
        "name": snapshot_desc["details"].get("executable_name"),
        "version": snapshot_desc["details"].get("executable_version")
    }

    # check if name and version of exec are already listed
    # if not, find the id
    if not snapshot_exec_desc["name"] or not snapshot_exec_desc["version"]:
        snapshot_exec_desc["id"] = snapshot_exec_desc.get("id") \
            or snapshot_desc["createdBy"].get("executable") \
            or dxpy.describe(snapshot_desc["createdBy"]["job"]).get("executable")

    return snapshot_exec_desc

def check_snapshot_version(snapshot_input: str) -> str:
    """
    Compare the name and the major version of the running executable with the one that generated the target snapshot file

    :param snapshot_input: file ID or path of target snapshot file
    :type snapshot_input: str
    :return: warning message if the names and major versions of the executables do not match. 
    :rtype: str
    """
    try:
        running_exec_desc = get_running_exec_desc()
        if not running_exec_desc.get("name") or not running_exec_desc.get("version"):
            print(f"Failed to get the running executable details. The snapshot file may not be compatible with the running executable.")
            return

        snapshot_exec_desc = get_snapshot_exec_desc(snapshot_input)

        if running_exec_desc.get("id") == snapshot_exec_desc.get("id"):
            print(f"Snapshot file {snapshot_input} is generated by the current running executable.")
            return
        warning_message = compare_exec_name_version(running_exec_desc, snapshot_exec_desc, snapshot_input)
        if warning_message:
            logger.warning(warning_message)
        print("Checking snapshot version completed.")
    except Exception as e:
        logger.error(f"Checking snapshot version failed: {str(e)}")

    return

def compare_exec_name_version (running_exec_desc, snapshot_exec_desc, snapshot_input):
    running_exec_name = running_exec_desc.get("name")
    running_exec_version = running_exec_desc.get("version")
    snapshot_exec_name = snapshot_exec_desc.get("name")
    snapshot_exec_version = snapshot_exec_desc.get("version")

    if not snapshot_exec_name:
        return f"Snapshot file {snapshot_input} does not have 'executable_name' field in its details " \
            f"and may not be compatible with this executable."

    if snapshot_exec_name != running_exec_name:
        return f"Snapshot file {snapshot_input} has 'executable_name' field in its details set to {snapshot_exec_name}, " \
            f"which does not equal {running_exec_name}. The snapshot file may not be compatible with this executable."
        

    # snapshot_exec_name is the same as running_exec_name
    # continue to check the major version
    if not snapshot_exec_version:
        try:
            snapshot_exec_version = dxpy.describe(snapshot_exec_desc["id"]).get("version")
        except:
            pass        

    snapshot_exec_major_ver = dx_utils.get_major_version(snapshot_exec_version)
    running_exec_major_ver = dx_utils.get_major_version(running_exec_version)

    if not snapshot_exec_major_ver:
        return f"Failed to parse semantic version from '{snapshot_exec_version}' for snapshot file {snapshot_input}. " \
            f"The snapshot file may not be compatible with this executable."

    if not running_exec_major_ver:
        return f"Failed to parse semantic version from current executable version of '{running_exec_version}'. " \
            f"The snapshot file may not be compatible with this executable."

    if snapshot_exec_major_ver != running_exec_major_ver:
        return f"Snapshot file {snapshot_input} was generated by {snapshot_exec_name}/{snapshot_exec_version} and " \
            f"may not be compatible with the running {running_exec_name}/{running_exec_version}. " \
            f"If snapshot installation fails with this version of {running_exec_name}, please use the snapshot " \
            f"with {running_exec_name}/{snapshot_exec_version}."
    else:
        print(f"Snapshot executable: {snapshot_exec_name}/{snapshot_exec_version}")
        print(f"Running executable: {running_exec_name}/{running_exec_version}")
        return None

if __name__ == '__main__':
    args = get_parsed_args()
    if args.task == 'asset':
        if args.title is None or args.description is None or args.version is None:
            print('Assets require a title, description, and version.')
            sys.exit(1)
        try:
            asset_id, asset_path = create_asset(
                args.name, args.title, args.description, args.version)
            print("Created asset: {} ({})".format(asset_path, asset_id))
        except Exception as e:
            print("Asset creation failed with {}\nPlease contact support@dnanexus.com".format(str(e)))
            sys.exit(1)

    elif args.task == 'snapshot':
        try:
            snapshot_id, snapshot_path = create_snapshot(
                args.name, DEFAULT_IGNORE_PATH)
            print("Created snapshot: {} ({})".format(
                snapshot_path, snapshot_id))
        except Exception as e:
            print("Snapshot creation failed with {}\nPlease contact support@dnanexus.com".format(str(e)))
            sys.exit(1)

    elif args.task == 'check_version':
        if not args.snapshot_input:
            print('Checking snapshot version requires a valid snapshot file ID/path.')
            sys.exit(1)
        check_snapshot_version(args.snapshot_input)
