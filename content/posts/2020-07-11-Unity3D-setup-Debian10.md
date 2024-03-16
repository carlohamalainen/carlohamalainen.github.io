---
author: Carlo Hamalainen
date: "2020-07-11T00:00:00Z"
title: Unity3D setup on Debian 10 including Visual Studio Code Intellisense
url: /2020/07/11/unity3d-setup-debian10-intellisense/
---

The end goal here is to have Unity3D and Visual Studio Code installed on a Debian 10 Linux system with Intellisense completions
working for Unity code.

You should be able to type the start of a Unity name like ``GameObject`` and see the completion:

{{< figure src="/stuff/unity3d-2020-07-11/unity_code_intellisense_zoomed.png" >}}

I mostly followed [@nosuchstudio's guide for Ubuntu 18.04 LTS](https://medium.com/@nosuchstudio/setting-up-unity-2019-x-with-vscode-on-ubuntu-18-04-lts-a-troubleshooting-guide-75da7ff29ff5) but added some extra details and confirmed Debian 10 compatability.

There are four things to install and configure:

1. Unity Hub
2. Visual Studio Code
3. .NET Core 3.1
4. Mono

## 1. Install Unity Hub

Download the AppImage from [here](https://docs.unity3d.com/2019.2/Documentation/Manual/GettingStartedInstallingHub.html). Move it
to a convenient location and set it executable:

    mkdir ~/opt/unity
    mv Downloads/UnityHub.AppImage ~/opt/unity
    chmod +x opt/unity/UnityHub.AppImage

## 2. Install Visual Studio Code

Download the [64bit deb of Visual Studio Code](https://code.visualstudio.com/download#) and install it:

    sudo dpkg -i Downloads/code_1.47.0-1594283939_amd64.deb

Make sure that you have the C# extension installed:

{{< figure src="/stuff/unity3d-2020-07-11/code_extensions.png" >}}

Also make sure that under Unity3D's Edit/Preferences/External Tools menu, you have ``Visual Studio Code`` selected
(not ``Open by file extension``) and all the checkboxes ticked:

{{< figure src="/stuff/unity3d-2020-07-11/unity_preferences.png" >}}

## 3. Install .NET Core 3.1

Debian 10 can run ``.NET Core 3.1`` (see [these](https://docs.microsoft.com/en-us/dotnet/core/install/linux#debian) notes).

Following the [detailed installation steps](https://docs.microsoft.com/en-us/dotnet/core/install/linux-debian):

    wget https://packages.microsoft.com/config/debian/10/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
    sudo dpkg -i packages-microsoft-prod.deb

    sudo apt-get update; \
      sudo apt-get install -y apt-transport-https && \
      sudo apt-get update && \
      sudo apt-get install -y dotnet-sdk-3.1

## 4. Install Mono

Follow the [Debian instructions](https://www.mono-project.com/download/stable/#download-lin-debian):

    sudo apt install apt-transport-https dirmngr gnupg ca-certificates
    sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
    echo "deb https://download.mono-project.com/repo/debian stable-buster main" | sudo tee /etc/apt/sources.list.d/mono-official-stable.list
    sudo apt update
    sudo apt install mono-complete

## Test the integration and Intellisense

Create a new Unity3D project and add a C# script in your Assets folder:

{{< figure src="/stuff/unity3d-2020-07-11/unity_assets_MyScript.png" >}}

If you have a script attached to a game object, do NOT click the ``Edit script`` menu item as this
will load the invidivual file. Instead, open the script via the Assets window and select
``Open C# Project``:

{{< figure src="/stuff/unity3d-2020-07-11/unity_open_cs_project.png" >}}

Intellisense won't work until the project has been compiled. You'll see lots of ``mono`` processes:

{{< figure src="/stuff/unity3d-2020-07-11/unity_mono_processes.png" >}}

Once everything has compiled, test out Intellisense completions:

{{< figure src="/stuff/unity3d-2020-07-11/unity_code_intellisense.png" >}}

Also note that the left panel has lots of Unity components, not just the C# file. This indicates that you have
opened the C# project, not an individual file.
