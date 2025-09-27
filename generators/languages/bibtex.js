/**
 * BibTeX Language Code Base Generator
 * Generates BibTeX code base/boilerplate code
 */

/**
 * Generates BibTeX code base
 * @returns {string} BibTeX code base template
 */
function generateBibTeXCodeBase() {
    return `@article{example2025,
  title={Example Article Title},
  author={Author, First and Author, Second},
  journal={Journal of Examples},
  volume={1},
  number={1},
  pages={1--10},
  year={2025},
  publisher={Example Publisher}
}

@book{exampleBook2025,
  title={Example Book Title},
  author={Author, First},
  year={2025},
  publisher={Example Publisher},
  address={Example City}
}

@inproceedings{exampleConference2025,
  title={Example Conference Paper},
  author={Author, First},
  booktitle={Proceedings of the Example Conference},
  pages={100--110},
  year={2025},
  organization={Example Organization}
}`;
}

module.exports = {
    generateBibTeXCodeBase
};