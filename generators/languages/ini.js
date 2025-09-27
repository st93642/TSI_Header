/**
 * INI Code Base Generator
 * Generates boilerplate INI configuration files
 */

function generateIniCodeBase() {
    return `\n; Basic INI configuration file template\n\n[Application]\nName=TSI Header Application\nVersion=1.0.0\nDescription=Basic application configuration\n\n[Database]\nHost=localhost\nPort=5432\nName=tsi_database\nUser=tsi_user\n; Password=your_password_here\n\n[Settings]\nDebug=true\nLogLevel=info\nMaxConnections=100\nTimeout=30\n\n[Paths]\nDataDir=/var/data\nLogDir=/var/log\nTempDir=/tmp\n\n; Comments can use semicolons or hash symbols\n# This is also a valid comment\n\n[Features]\nFeature1=enabled\nFeature2=disabled\nExperimentalFeatures=false\n`;
}

module.exports = {
    generateIniCodeBase
};