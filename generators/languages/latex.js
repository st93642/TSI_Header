/**
 * LaTeX Code Base Generator
 * Generates boilerplate LaTeX documents
 */

function generateLatexCodeBase() {
    return `\n\\documentclass[12pt,a4paper]{article}\n\n% Basic packages\n\\usepackage[utf8]{inputenc}\n\\usepackage[T1]{fontenc}\n\\usepackage{amsmath,amsfonts,amssymb}\n\\usepackage{graphicx}\n\\usepackage[margin=2.5cm]{geometry}\n\\usepackage{hyperref}\n\n% Document information\n\\title{TSI Header - Basic LaTeX Template}\n\\author{TSI Student}\n\\date{\\today}\n\n\\begin{document}\n\n\\maketitle\n\n\\begin{abstract}\nThis is a basic LaTeX document template for Transport and Telecommunication Institute students. It includes common packages and a standard structure for academic documents.\n\\end{abstract}\n\n\\section{Introduction}\nHello, World! This is a basic LaTeX document.\n\n\\section{Main Content}\nThis template provides a foundation for creating professional academic documents.\n\n\\subsection{Features}\n\\begin{itemize}\n    \\item Professional document formatting\n    \\item Mathematical notation support\n    \\item Graphics and table support\n    \\item Bibliography support (add \\texttt{biblatex} if needed)\n\\end{itemize}\n\n\\section{Mathematics}\nHere's an example equation:\n\\begin{equation}\n    E = mc^2\n\\end{equation}\n\n\\section{Conclusion}\nThis concludes the basic LaTeX template.\n\n\\end{document}\n`;
}

module.exports = {
    generateLatexCodeBase
};