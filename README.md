[![GitHub](https://img.shields.io/badge/GitHub-ForOpenAI-blue.svg?style=social&logo=github)](https://github.com/gha3mi/foropenai)
[![Version](https://img.shields.io/github/v/tag/gha3mi/foropenai?color=blue&logo=github&style=flat)](https://github.com/gha3mi/foropenai/releases)
[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://gha3mi.github.io/foropenai/)
[![License](https://img.shields.io/github/license/gha3mi/foropenai?color=green)](https://github.com/gha3mi/foropenai/blob/main/LICENSE)
[![Build](https://github.com/gha3mi/foropenai/actions/workflows/ci.yml/badge.svg)](https://github.com/gha3mi/foropenai/actions/workflows/ci.yml)

<img alt="ForOpenAI" src="https://github.com/gha3mi/foropenai/raw/main/media/logo.png" width="750">

**ForOpenAI**: A community-maintained Fortran wrapper for the OpenAI API.

## How to use

**Prerequisites:**

On Ubuntu, you need to install the curl development headers. Use the following command:

```shell
sudo apt install -y libcurl4-openssl-dev
```

**Clone the repository:**

You can clone the ForOpenAI repository from GitHub using the following command:

```shell
git clone https://github.com/gha3mi/foropenai.git
```

```shell
cd foropenai
```

**OpenAI API Key Configuration:**

Your Secret API key can be located by accessing the OpenAI [User settings](https://platform.openai.com/account/api-keys).

For enhanced security and convenience, it is strongly recommended to configure the API key as an environment variable. 

- On Ubuntu, use the following command, replacing `"your_api_key"` with your actual API key:

    ```shell
    export OPENAI_API_KEY="your_api_key"
    ```

- (Optional) If desired for organizational purposes, you can also establish an optional environment variable on Ubuntu:

    ```shell
    export OPENAI_ORG="your_organization"
    ```

- Alternatively, the OpenAI API key can be included in the `foropenai.json` configuration file.

    ```json
    {
        "base": {
            "api_key": "OPENAI_API_KEY",
            "organization": ""
        }
    }
    ```

**Use ChatGPT from the terminal:**

```shell
fpm run
```

**Example**

![Alt text](media/example.png)

## Create Chat Completion

```fortran
program test_ChatCompletion

   use foropenai, only: ChatCompletion

   implicit none

   type(ChatCompletion) :: chat

   call chat%set_base_data(file_name='foropenai.json')
   call chat%set(file_name='foropenai.json')

   call chat%init_messages(n=3)
   call chat%messages(1)%set(role='system', content='You are a helpful assistant.')
   call chat%messages(2)%set(role='user', content='Hello?')
   call chat%messages(3)%set(role='assistant', content='')

   call chat%print_user_message()
   call chat%create()
   call chat%print_assistant_response()

   call chat%usage%print()

   call chat%finalize()

end program test_ChatCompletion
```

### Result:

```shell
Ali: Hello?
ChatGPT: Hello! How can I assist you today?
```

## Transcription
<audio src="test/audio.mp3" controls title="Title"></audio>
```fortran
program test_Transcription

   use foropenai, only: Transcription

   implicit none

   type(Transcription) :: trs

   call trs%set_base_data(file_name='foropenai.json')
   call trs%set(file_name='foropenai.json')

   call trs%create(file='test/audio.mp3')
   call trs%print_text()

   call trs%finalize()

end program test_Transcription
```
### Result:

```shell
text: FORTRAN stands for Formula Translation.
```

## Image Generation

```fortran
program test_ImageGeneration

   use foropenai, only: ImageGeneration

   implicit none

   type(ImageGeneration) :: image

   call image%set_base_data(file_name='foropenai.json')
   call image%set(file_name='foropenai.json')

   call image%create(prompt='a cat with a computer')
   call image%print_assistant_response()

   call image%finalize()

end program test_ImageGeneration
```

### Result:

<img src="image.png" alt="Alt text" width="200" height="" />

## fpm dependency

If you want to use `ForOpenAI` as a dependency in your own fpm project,
you can easily include it by adding the following line to your `fpm.toml` file:

```toml
[dependencies]
foropenai = {git="https://github.com/gha3mi/foropenai.git"}
```

## API documentation

The most up-to-date API documentation for the master branch is available
[here](https://gha3mi.github.io/foropenai/).
To generate the API documentation for `ForOpenAI` using
[ford](https://github.com/Fortran-FOSS-Programmers/ford) run the following
command:

```shell
ford ford.yml
```

## Contributing

Contributions to `ForOpenAI` are welcome!
If you find any issues or would like to suggest improvements, please open an issue.
