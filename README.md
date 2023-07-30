[![GitHub](https://img.shields.io/badge/GitHub-ForOpenAI-blue.svg?style=social&logo=github)](https://github.com/gha3mi/foropenai)
[![Version](https://img.shields.io/github/v/tag/gha3mi/foropenai?color=blue&logo=github&style=flat)](https://github.com/gha3mi/foropenai/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/foropenai/)
[![License](https://img.shields.io/github/license/gha3mi/foropenai?color=green)](https://github.com/gha3mi/foropenai/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/foropenai/actions/workflows/ci.yml/badge.svg)](https://github.com/gha3mi/foropenai/actions/workflows/ci.yml)

<img alt="ForOpenAI" src="https://github.com/gha3mi/foropenai/raw/main/media/logo.png" width="750">

**ForOpenAI**: A Fortran library for OpenAI API. (Under Development!)

## How to use

**Prerequisites:**

On Ubuntu, you need to install the curl development headers. Use the following command:

```shell
sudo apt install -y libcurl4-openssl-dev
```

**OpenAI API key:**

Create a file named `.foropenai` and insert your OpenAI API key. You can find your Secret API key in your OpenAI [User settings](https://platform.openai.com/account/api-keys). 

Make sure to keep the file name exactly as mentioned, starting with a dot (.).

**Clone the repository:**

You can clone the ForOpenAI repository from GitHub using the following command:

```shell
git clone https://github.com/gha3mi/foropenai.git
```

```shell
cd foropenai
```

**Use ChatGPT from the terminal:**

```shell
fpm run
```

## fpm dependency

If you want to use ForOpenAI as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
foropenai = {git="https://github.com/gha3mi/foropenai.git"}
```

## API documentation

To generate the API documentation for the `ForOpenAI` module using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing
Contributions to `ForOpenAI` are welcome! If you find any issues or would like to suggest improvements, please open an issue.