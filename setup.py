from distutils.core import setup

setup(
    name = "popsicle",
    version = "1.0",
    description = "Type rules to LaTeX",
    author = "Nicholas Matsakis",
    author_email = "niko@alum.mit.edu",
    packages = ['popsicle_mod'],
    scripts = ['popsicle']
)