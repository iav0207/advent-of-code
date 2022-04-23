#!/bin/bash

scripts_dir="$(dirname $0)"
project_root=$scripts_dir/..
lib_dir=$project_root/lib

mkdir -p $lib_dir

function pkg_url() {
    group=$1 artifact=$2
    echo "https://repo1.maven.org/maven2/${group//.//}/${artifact}"
}

function get_available_versions() {
    meta_url="$(pkg_url $@)/maven-metadata.xml"
    curl $meta_url | grep '<version>' | cut -d'>' -f2 | cut -d'<' -f1
}

function download_jar() {
    group=$1 artifact=$2 version=$3
    jar_name="${artifact}-${version}.jar"
    jar_url="$(pkg_url $group $artifact)/${version}/${jar_name}"

    echo Downloading $artifact version $version
    curl -s -o $lib_dir/$jar_name $jar_url
}

download_jar org.jetbrains.kotlin kotlin-stdlib 1.6.21

for kt_src_dir in $(dirname $(find $project_root -iname "*.kt") | sort -u); do
    lib_link=$kt_src_dir/lib
    if [[ -d $lib_link ]] || [[ -L $lib_link ]]; then
        echo
        echo $lib_link already exists:
        while read line; do echo "    $line"; done < <(ls -l $lib_link)
        echo
    else
        echo "Linking $lib_link -> $lib_dir"
        ln -s $lib_dir $kt_src_dir/lib
    fi
done

