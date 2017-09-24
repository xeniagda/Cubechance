import subprocess
import os

for elm in os.listdir("."):
    if not elm.endswith(".elm"):
        continue
    cmd = ["elm-make", elm, "--output", "../Static/" + elm.replace(".elm", ".js")]
    subprocess.run(cmd)