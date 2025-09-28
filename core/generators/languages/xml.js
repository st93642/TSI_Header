/**
 * XML Code Base Generator
 * Generates boilerplate XML code
 */

function generateXmlCodeBase() {
    return `\n<?xml version="1.0" encoding="UTF-8"?>\n<!-- Basic XML document template -->\n\n<tsi:document xmlns:tsi="https://tsi.lv/schema" \n              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n              xsi:schemaLocation="https://tsi.lv/schema tsi-schema.xsd">\n  \n  <!-- Document metadata -->\n  <tsi:metadata>\n    <tsi:title>TSI Header - Basic XML Template</tsi:title>\n    <tsi:author>TSI Student</tsi:author>\n    <tsi:created>2025-09-24</tsi:created>\n    <tsi:version>1.0</tsi:version>\n  </tsi:metadata>\n  \n  <!-- Main content -->\n  <tsi:content>\n    <tsi:greeting>Hello, World!</tsi:greeting>\n    <tsi:description>This is a basic XML document.</tsi:description>\n    \n    <!-- Institution information -->\n    <tsi:institution>\n      <tsi:name>Transport and Telecommunication Institute</tsi:name>\n      <tsi:location>Riga, Latvia</tsi:location>\n      <tsi:website>https://tsi.lv</tsi:website>\n      \n      <tsi:programs>\n        <tsi:program id="cs" level="bachelor">\n          <tsi:name>Computer Science</tsi:name>\n          <tsi:duration>4 years</tsi:duration>\n        </tsi:program>\n        <tsi:program id="ee" level="bachelor">\n          <tsi:name>Electrical Engineering</tsi:name>\n          <tsi:duration>4 years</tsi:duration>\n        </tsi:program>\n      </tsi:programs>\n    </tsi:institution>\n    \n    <!-- Example data structure -->\n    <tsi:courses>\n      <tsi:course code="CS101">\n        <tsi:title>Programming Fundamentals</tsi:title>\n        <tsi:credits>6</tsi:credits>\n        <tsi:semester>1</tsi:semester>\n      </tsi:course>\n      <tsi:course code="MATH201">\n        <tsi:title>Advanced Mathematics</tsi:title>\n        <tsi:credits>4</tsi:credits>\n        <tsi:semester>2</tsi:semester>\n      </tsi:course>\n    </tsi:courses>\n  </tsi:content>\n  \n</tsi:document>\n`;
}

module.exports = {
    generateXmlCodeBase
};