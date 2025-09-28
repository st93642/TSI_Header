/**
 * PHP Project Creator
 * Creates PHP-specific project files and structure
 */

const { generateTSIHeaderContent } = require('../headerUtils');

/**
 * Create PHP-specific files
 */
async function createPhpFiles(vscode, projectName, projectUri) {
    await createComposerJson(vscode, projectName, projectUri);
    await createBaseClassPhp(vscode, projectName, projectUri);
}

/**
 * Create composer.json file
 */
async function createComposerJson(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'composer.json');

    const content = `{
    "name": "${projectName.toLowerCase().replace(/-/g, '_')}",
    "description": "A ${projectName} project created with TSI Header",
    "type": "project",
    "license": "MIT",
    "authors": [
        {
            "name": "TSI Student",
            "email": "student@tsi.lv"
        }
    ],
    "require": {
        "php": ">=8.0"
    },
    "require-dev": {
        "phpunit/phpunit": "^9.5",
        "phpstan/phpstan": "^1.8"
    },
    "autoload": {
        "psr-4": {
            "${projectName}\\\\": "src/"
        }
    },
    "autoload-dev": {
        "psr-4": {
            "${projectName}\\\\Tests\\\\": "tests/"
        }
    },
    "scripts": {
        "test": "phpunit",
        "analyze": "phpstan analyse",
        "start": "php -S localhost:8000 -t public/"
    },
    "config": {
        "optimize-autoloader": true,
        "sort-packages": true
    }
}`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create BaseClass.php file for PHP projects
 */
async function createBaseClassPhp(vscode, projectName, projectUri) {
    const fileUri = vscode.Uri.joinPath(projectUri, 'src', 'BaseClass.php');

    // Generate TSI header
    const headerContent = await generateTSIHeaderContent('BaseClass.php', vscode);

    const content = `${headerContent}

<?php

/**
 * BaseClass - Foundation class for ${projectName}
 *
 * This class provides basic functionality and serves as a base
 * for other classes in the ${projectName} project.
 */

namespace ${projectName};

class BaseClass
{
    protected $name;
    protected $id;
    private static $nextId = 1;

    public function __construct($name = "DefaultObject", $id = null)
    {
        $this->name = $name ?: "UnnamedObject";
        $this->id = $id ?? self::$nextId++;
    }

    public function getName()
    {
        return $this->name;
    }

    public function getId()
    {
        return $this->id;
    }

    public function display()
    {
        echo "BaseClass Object: {$this->name} (ID: {$this->id})\\n";
    }
}

?>`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

module.exports = {
    createPhpFiles
};
