/**
 * Blockr — shared namespace and utilities for blockr JS components.
 * Must be loaded before any component files (blockr-select.js, etc.).
 */
window.Blockr = window.Blockr || {};

Blockr.uid = (() => {
  let counter = 0;
  return (prefix = 'b') => `${prefix}-${++counter}`;
})();

Blockr.escapeHtml = (s) => {
  const div = document.createElement('div');
  div.appendChild(document.createTextNode(s));
  return div.innerHTML;
};

Blockr.removeNode = (node) => {
  node?.parentNode?.removeChild(node);
};

Blockr.icons = {
  chevron:
    '<svg width="12" height="12" viewBox="0 0 12 12" fill="none" stroke="currentColor" ' +
    'stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round">' +
    '<polyline points="3 4.5 6 7.5 9 4.5"></polyline></svg>',
  remove:
    '<svg width="10" height="10" viewBox="0 0 10 10" fill="none" stroke="currentColor" ' +
    'stroke-width="1.5" stroke-linecap="round">' +
    '<line x1="2.5" y1="2.5" x2="7.5" y2="7.5"></line>' +
    '<line x1="7.5" y1="2.5" x2="2.5" y2="7.5"></line></svg>',
  x:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M2.146 2.854a.5.5 0 1 1 .708-.708L8 7.293l5.146-5.147a.5.5 0 0 1 ' +
    '.708.708L8.707 8l5.147 5.146a.5.5 0 0 1-.708.708L8 8.707l-5.146 5.147a.5.5 0 0 1-.708-.708L7.293 8z"/></svg>',
  plus:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M8 2a.5.5 0 0 1 .5.5v5h5a.5.5 0 0 1 0 1h-5v5a.5.5 0 0 1-1 ' +
    '0v-5h-5a.5.5 0 0 1 0-1h5v-5A.5.5 0 0 1 8 2"/></svg>',
  confirm:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 ' +
    '1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>',
  code:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M10.478 1.647a.5.5 0 1 0-.956-.294l-4 13a.5.5 0 0 0 .956.294z' +
    'M4.854 4.146a.5.5 0 0 1 0 .708L1.707 8l3.147 3.146a.5.5 0 0 1-.708.708l-3.5-3.5a.5.5 0 0 1 ' +
    '0-.708l3.5-3.5a.5.5 0 0 1 .708 0m6.292 0a.5.5 0 0 0 0 .708L14.293 8l-3.147 3.146a.5.5 0 0 0 ' +
    '.708.708l3.5-3.5a.5.5 0 0 0 0-.708l-3.5-3.5a.5.5 0 0 0-.708 0"/></svg>',
  gear:
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" fill="currentColor" ' +
    'viewBox="0 0 16 16"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 ' +
    '1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82' +
    '.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 ' +
    '.872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ' +
    '2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31' +
    '.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105l.34-' +
    '.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-' +
    '1.283-.705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ' +
    '2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
};
