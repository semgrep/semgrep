import setuptools

install_requires = [
    "pandas==1.4.2",
]


setuptools.setup(
    name="dummy-package",
    version="1.2.0",
    packages=setuptools.find_packages(where="src"),
    install_requires=install_requires,
)
