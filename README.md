![ForOpenAI](media/logo.png)
============

**ForOpenAI**: A Fortran library for OpenAI API. (Under Development!)

## How to use

**Prerequisites:**

On Ubuntu, you need to install the curl development headers. Use the following command:

```shell
sudo apt install -y libcurl4-openssl-dev
```

### fpm dependency

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
