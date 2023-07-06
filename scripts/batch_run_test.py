import os
import sys
import logging
import subprocess

NOOP_HOME = os.getenv("NOOP_HOME")
NEMU_HOME = os.getenv("NEMU_HOME")
AM_HOME = os.getenv("AM_HOME")
ISA = "la32r"
PLATFORM = "nutshell"

if NOOP_HOME == None or NEMU_HOME == None or AM_HOME == None:
    sys.stderr.write("Environment variables are not set correctly\n")
    raise SystemExit(1)

emu_path = os.path.join(NOOP_HOME, "build/emu")
nemu_path = os.path.join(NEMU_HOME, "build/", ISA + "-nemu-interpreter-so")
print(emu_path)

if not os.path.exists(emu_path) or not os.path.exists(nemu_path):
    sys.stderr.write("nemu or emu are not set correctly\n")
    raise SystemExit(1)

cputest_dir = os.path.join(AM_HOME, "tests/cputest/build")
# command : ls | awk '{print substr($0, 1, length($0) - 2)}' | awk '{print "\""$0"\""}' | awk '{print $0 ","}'
cputest_list = ["add-longlong",
                "add",
                "bit",
                "bubble-sort",
                "div",
                "dummy",
                "fact",
                "fib",
                "goldbach",
                "hello-str",
                "if-else",
                "leap-year",
                "load-store-check",
                "load-store",
                "matrix-mul",
                "max",
                "min3",
                "mov-c",
                "movsx",
                "mul-longlong",
                "pascal",
                "prime",
                "quick-sort",
                "recursion",
                "select-sort",
                "shift",
                "shuixianhua",
                "string",
                "sub-longlong",
                "sum",
                "switch",
                "to-lower-case",
                "unalign",
                "wanshu",
]


runlog = open("./run.log",'wt')
logpath = sys.path[0] + "/run.log"

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)
formatter = logging.Formatter(
    '%(asctime)s - %(name)s - %(levelname)s: - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S')
# 使用FileHandler输出到文件
fh = logging.FileHandler('run.log')
fh.setLevel(logging.DEBUG)
fh.setFormatter(formatter)

# 使用StreamHandler输出到屏幕
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
ch.setFormatter(formatter)

# 添加两个Handler
logger.addHandler(ch)
logger.addHandler(fh)

print("begin batch run test, output to " + logpath)

# cputest
cputestnum = len(cputest_list)
passcputestnum = 0
logging.debug("begin cputest")

for tp in cputest_list:
    tp_path = os.path.join(cputest_dir, tp + "-" + ISA + "-" + PLATFORM + ".bin")
    if not os.path.exists(tp_path):
        logging.debug(tp_path + " does not exists, skip")
        continue
    runcommand = emu_path + " -i " + tp_path + " -b 0 -e 0 -l 0"
    logging.debug("runcommand:" + runcommand)
    try:
        out_bytes = subprocess.check_output([emu_path, "-i", tp_path], stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        out_bytes = e.output
        print(out_bytes.decode('utf-8'), file=runlog)
        exit(1)
    print(out_bytes.decode('utf-8'), file=runlog)

logging.debug("all cputest pass")
runlog.close()