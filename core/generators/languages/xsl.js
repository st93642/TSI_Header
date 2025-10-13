/**
 * XSL Code Base Generator
 * Generates boilerplate XSL code
 */

function generateXslCodeBase() {
    return `\n<?xml version="1.0" encoding="UTF-8"?>\n<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">\n\n<!-- Basic XSL transformation template -->\n<xsl:output method="html" encoding="UTF-8" indent="yes" />\n\n<!-- Main template match -->\n<xsl:template match="/">\n    <html>\n        <head>\n            <title>Uni-header - Basic XSL Template</title>\n        </head>\n        <body>\n            <h1>Hello, World!</h1>\n            <p>This is a basic XSL transformation.</p>\n            \n            <!-- Process document content -->\n            <xsl:apply-templates select="//content" />\n        </body>\n    </html>\n</xsl:template>\n\n<!-- Template for content elements -->\n<xsl:template match="content">\n    <div class="content">\n        <h2><xsl:value-of select="@title" /></h2>\n        <p><xsl:value-of select="." /></p>\n    </div>\n</xsl:template>\n\n<!-- Example: Transform list items -->\n<xsl:template match="items">\n    <ul>\n        <xsl:for-each select="item">\n            <li><xsl:value-of select="." /></li>\n        </xsl:for-each>\n    </ul>\n</xsl:template>\n\n</xsl:stylesheet>\n`;
}

module.exports = {
    generateXslCodeBase
};