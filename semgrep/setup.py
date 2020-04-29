# type: ignore
import setuptools

setuptools.setup(
    name="semgrep",  # Replace with your own username
    version="0.5.0",
    author="Russell & Return 2 Corp",
    author_email="author@example.com",
    description="semgrep python wrapper",
    long_description="bloop",
    long_description_content_type="text/markdown",
    url="https://github.com/pypa/sampleproject",
    install_requires=["colorama==0.4.3", "pyyaml==5.3", "requests==2.22.0"],
    entry_points={"console_scripts": ["semgrep=semgrep.__main__:main"]},
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.7",
)
