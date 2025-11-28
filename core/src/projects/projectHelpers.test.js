const { describe, it, beforeEach } = require('node:test');
const assert = require('node:assert');
const {
    getDirectoryStructure,
    getFileExtension
} = require('./projectHelpers');

describe('ProjectHelpers', () => {
    describe('getDirectoryStructure', () => {
        it('should return correct directory structure for C projects', () => {
            const dirs = getDirectoryStructure('c');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'include', 'build']);
        });

        it('should return correct directory structure for C++ projects', () => {
            const dirs = getDirectoryStructure('cpp');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'include', 'build']);
        });

        it('should return correct directory structure for Python projects', () => {
            const dirs = getDirectoryStructure('python');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'tests', 'scripts']);
        });

        it('should return correct directory structure for Java projects', () => {
            const dirs = getDirectoryStructure('java');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'src/main/java', 'src/test/java', 'target']);
        });

        it('should return correct directory structure for Rust projects', () => {
            const dirs = getDirectoryStructure('rust');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'src', 'tests', 'examples', 'benches']);
        });

        it('should return correct directory structure for Ruby projects', () => {
            const dirs = getDirectoryStructure('ruby');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'lib', 'spec', 'bin', 'config']);
        });

        it('should return correct directory structure for PHP projects', () => {
            const dirs = getDirectoryStructure('php');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'src', 'public', 'tests']);
        });

        it('should return correct directory structure for HTML projects', () => {
            const dirs = getDirectoryStructure('html');
            assert.deepStrictEqual(dirs, ['src', 'docs', 'src', 'assets', 'css', 'js']);
        });

        it('should return common directories for unknown language', () => {
            const dirs = getDirectoryStructure('unknown');
            assert.deepStrictEqual(dirs, ['src', 'docs']);
        });
    });

    describe('getFileExtension', () => {
        it('should return correct extension for C', () => {
            assert.strictEqual(getFileExtension('c'), 'c');
        });

        it('should return correct extension for C++', () => {
            assert.strictEqual(getFileExtension('cpp'), 'cpp');
        });

        it('should return correct extension for Python', () => {
            assert.strictEqual(getFileExtension('python'), 'py');
        });

        it('should return correct extension for Java', () => {
            assert.strictEqual(getFileExtension('java'), 'java');
        });

        it('should return correct extension for Rust', () => {
            assert.strictEqual(getFileExtension('rust'), 'rs');
        });

        it('should return correct extension for Ruby', () => {
            assert.strictEqual(getFileExtension('ruby'), 'rb');
        });

        it('should return correct extension for PHP', () => {
            assert.strictEqual(getFileExtension('php'), 'php');
        });

        it('should return correct extension for HTML', () => {
            assert.strictEqual(getFileExtension('html'), 'html');
        });

        it('should return txt for unknown language', () => {
            assert.strictEqual(getFileExtension('unknown'), 'txt');
        });
    });
});
