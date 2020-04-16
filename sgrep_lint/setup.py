import setuptools

setuptools.setup(
    name="sgrep", # Replace with your own username
    version="0.0.4",
    author="Russell & Return 2 Corp",
    author_email="author@example.com",
    description="sgrep python wrapper",
    long_description="bloop",
    long_description_content_type="text/markdown",
    url="https://github.com/pypa/sampleproject",
    install_requires=[
	'colorama==0.4.3',
	'pyyaml==5.3',
	'requests==2.22.0'
    ],
    entry_points={
      'console_scripts': ['semgrep=sgrep:main']
    },
    py_modules=["sgrep", "config_resolver", "constants", "evaluation", "sgrep_main", "sgrep_types", "util"],
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.7',
)
