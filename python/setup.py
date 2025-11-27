"""Setup file for minigst Python package."""

from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="minigst",
    version="0.1.0",
    author="Emilie Chautru, Mike Pereira, Thomas Romary",
    author_email="mike.pereira@minesparis.psl.eu",
    description="Wrapper package for gstlearn - Python version",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/mike-pereira/minigst",
    packages=find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License (GPL)",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.8",
    install_requires=[
        "gstlearn>=1.4.0",
        "matplotlib>=3.5.0",
        "numpy>=1.20.0",
        "pandas>=1.3.0",
    ],
    include_package_data=True,
    package_data={
        "minigst": ["datafiles/**/*"],
    },
)
