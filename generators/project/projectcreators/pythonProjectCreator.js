/**
 * Python Project Creator
 * Creates Python-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create Python-specific files
 */
async function createPythonFiles(vscode, projectName, projectUri) {
    await createRequirementsTxt(vscode, projectName, projectUri);
    await createSetupPy(vscode, projectName, projectUri);
    await createBaseClassPy(vscode, projectName, projectUri);
}

/**
 * Create requirements.txt file
 */
async function createRequirementsTxt(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'requirements.txt');

    const content = `# ${projectName} requirements
# Add your project dependencies here
# Example:
# requests>=2.25.1
# numpy>=1.21.0
# pandas>=1.3.0
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create setup.py file
 */
async function createSetupPy(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'setup.py');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('setup.py', vscode);

    const content = `${headerContent}

"""
Setup script for ${projectName}
"""

from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="${projectName}",
    version="0.1.0",
    author="TSI Student",
    author_email="student@tsi.lv",
    description="A ${projectName} project created with TSI Header",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/yourusername/${projectName}",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
    ],
    python_requires=">=3.8",
    install_requires=[
        # Add your dependencies here
    ],
    extras_require={
        "dev": [
            "pytest>=6.0",
            "pytest-cov>=2.0",
            "black>=21.0",
            "flake8>=3.9",
            "mypy>=0.910",
        ],
    },
)
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.py file for Python projects
 */
async function createBaseClassPy(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'base_class.py');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('base_class.py', vscode);

    const content = `${headerContent}

"""
BaseClass - Foundation class for ${projectName}

This class provides basic functionality and serves as a base
for other classes in the ${projectName} project.

Features:
- Basic object initialization
- String-based naming system
- Unique ID generation
- Display functionality
- JSON serialization support
"""

import json
from typing import Dict, Any, Optional
from datetime import datetime


class BaseClass:
    """Base class providing common functionality for ${projectName} objects."""

    # Class variable for ID generation
    _next_id = 1

    def __init__(self, name: str = "DefaultObject", obj_id: Optional[int] = None):
        """
        Initialize BaseClass object.

        Args:
            name: Name for this object
            obj_id: Optional specific ID, auto-generated if not provided
        """
        self._name = name if name else "UnnamedObject"
        self._id = obj_id if obj_id is not None else BaseClass._next_id
        if obj_id is None or obj_id >= BaseClass._next_id:
            BaseClass._next_id = self._id + 1

        self._created_at = datetime.now()
        self._initialize()

    def _initialize(self) -> None:
        """Initialize common object properties."""
        pass

    @property
    def name(self) -> str:
        """Get the object name."""
        return self._name

    @name.setter
    def name(self, new_name: str) -> None:
        """Set the object name."""
        if new_name and isinstance(new_name, str):
            self._name = new_name

    @property
    def id(self) -> int:
        """Get the object ID."""
        return self._id

    @classmethod
    def get_next_id(cls) -> int:
        """Get the next available ID."""
        return cls._next_id

    def display(self) -> None:
        """Display object information."""
        print(f"{self.__class__.__name__} Object:")
        print(f"  Name: {self._name}")
        print(f"  ID: {self._id}")
        print(f"  Created: {self._created_at.strftime('%Y-%m-%d %H:%M:%S')}")

    def to_dict(self) -> Dict[str, Any]:
        """Convert object to dictionary for serialization."""
        return {
            'name': self._name,
            'id': self._id,
            'class': self.__class__.__name__,
            'created_at': self._created_at.isoformat()
        }

    def to_json(self) -> str:
        """Convert object to JSON string."""
        return json.dumps(self.to_dict(), indent=2, default=str)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'BaseClass':
        """Create object from dictionary."""
        return cls(
            name=data.get('name', 'DefaultObject'),
            obj_id=data.get('id')
        )

    @classmethod
    def from_json(cls, json_str: str) -> 'BaseClass':
        """Create object from JSON string."""
        data = json.loads(json_str)
        return cls.from_dict(data)

    def __str__(self) -> str:
        """String representation of the object."""
        return f"{self.__class__.__name__}(name=\"{self._name}\", id={self._id})"

    def __repr__(self) -> str:
        """Detailed string representation."""
        return self.__str__()

    def __eq__(self, other: object) -> bool:
        """Check equality with another object."""
        if not isinstance(other, BaseClass):
            return False
        return self._name == other._name and self._id == other._id

    def __hash__(self) -> int:
        """Hash function for use in sets/dicts."""
        return hash((self._name, self._id))


# Example usage
if __name__ == "__main__":
    # Create some example objects
    obj1 = BaseClass("Example Object 1")
    obj2 = BaseClass("Example Object 2")

    print("Created objects:")
    obj1.display()
    print()
    obj2.display()
    print()

    print("JSON representation:")
    print(obj1.to_json())
    print()

    print("String representations:")
    print(f"str(): {str(obj1)}")
    print(f"repr(): {repr(obj1)}")
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

module.exports = {
    createPythonFiles
};