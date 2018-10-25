#!/bin/sh
if ! command -v gnat2goto; then
  echo >&2 "gnat2goto not on PATH!"
  exit 1
fi

ada_yaml_path="/tmp/AdaYaml"
if [ ! -d "$ada_yaml_path" ]; then
  echo "Grabbing AdaYaml..."
  if ! git clone https://www.github.com/yaml/AdaYaml.git "$ada_yaml_path" >/dev/null 2>/dev/null; then
    echo >&2 "Failed to download AdaYaml";
    exit 1
  fi
fi
gnat2goto -I "$ada_yaml_path/src/interface" -I "$ada_yaml_path/Parser_Tools/src/interface/" "$ada_yaml_path/src/implementation/yaml.adb"
