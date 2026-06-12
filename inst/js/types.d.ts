/**
 * Protocol types for blockr.dplyr's JS-driven blocks.
 *
 * This file is the contract between the R side (expr-builders.R,
 * js-block.R) and the JS side (blockr-core.js + per-block classes).
 * It is dev-tooling only: type-checked via tsconfig.json / `tsc`,
 * never shipped to the browser.
 */

/* --- Column metadata (R: build_column_summary / build_column_values /
       build_column_picker_meta) --- */

type BlockrColumnType = 'numeric' | 'integer' | 'logical' | 'character';

/** Lightweight summary sent eagerly on every data change. */
interface BlockrColumnSummary {
  name: string;
  type: BlockrColumnType;
  hasNA: boolean;
  /** attr(col, "label") or "" */
  label: string;
}

/** Full per-column metadata, fetched lazily on dropdown-open. */
interface BlockrColumnValues extends BlockrColumnSummary {
  min?: number;
  max?: number;
  /** numeric/integer columns */
  uniqueValues?: Array<number | string>;
  /** character-ish columns (factor order preserved by R) */
  values?: string[];
  hasEmpty?: boolean;
}

/** Name + label pair used by picker-style blocks. */
interface BlockrPickerColumn {
  name: string;
  label: string;
}

/* --- Filter block state (R: make_filter_expr) --- */

interface BlockrFilterCondition {
  type: 'values' | 'numeric' | 'expr';
  column?: string;
  /** 'values': picked values as strings; may include '<NA>' / '<empty>' */
  values?: string[];
  mode?: 'include' | 'exclude';
  /**
   * Column type tag so R converts values back to the column's type
   * instead of guessing by coercibility ("007" must stay a string).
   */
  colType?: BlockrColumnType | null;
  /** 'numeric': comparison operator */
  op?: string;
  value?: number;
  /** 'expr': free R expression */
  expr?: string;
}

interface BlockrFilterState {
  conditions: BlockrFilterCondition[];
  operator: '&' | '|';
  preserveOrder?: boolean;
}

/* --- Blockr.Select (blockr-select.js) --- */

/** Option entry: a bare value string, or {value, label} for a muted label. */
type BlockrSelectOption = string | { value: string; label?: string };

interface BlockrSelectConfigBase {
  options?: BlockrSelectOption[];
  /** Shown when nothing is selected (single) / no tags (multi). */
  placeholder?: string;
  /**
   * Start with the dropdown showing "Loading…" until setLoading(false).
   * Pairs with onOpen to lazily fetch the option list on first open.
   */
  loading?: boolean;
  /** Fires on every dropdown open. */
  onOpen?: () => void;
  /**
   * Cap on options that get DOM nodes per render (default 200). The full
   * list stays searchable; a "+N more" row reports the overflow.
   */
  maxRendered?: number;
}

interface BlockrSelectSingleConfig extends BlockrSelectConfigBase {
  /** Initial value (null/undefined: first option, or '' if none). */
  selected?: string | null;
  onChange?: (value: string) => void;
}

interface BlockrSelectMultiConfig extends BlockrSelectConfigBase {
  /** Initial values (copied, not aliased). */
  selected?: string[];
  /** Tags can be drag-reordered (default true). */
  reorderable?: boolean;
  /** Receives a copy of the selected values, in tag order. */
  onChange?: (value: string[]) => void;
}

/** Internal union as consumed by createSelect (mode picks the shape). */
interface BlockrSelectConfig extends BlockrSelectConfigBase {
  selected?: string | string[] | null;
  reorderable?: boolean;
  /** `any` so both per-mode signatures are assignable under strict variance. */
  onChange?: (value: any) => void;
}

interface BlockrSelectHandleBase {
  /** Root element (already appended to the container). */
  el: HTMLDivElement;
  /**
   * Replace the option list and reconcile the selection: single falls back
   * to the first option when `sel` is absent/unknown; multi keeps only
   * values present in the new options.
   */
  setOptions(
    opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined,
    sel?: string | string[] | null
  ): void;
  /**
   * Swap the option list without touching the current selection (setOptions
   * would drop chips whose value list hasn't arrived yet). Lazy loading.
   */
  updateOptions(opts: BlockrSelectOption[] | BlockrSelectOption | null | undefined): void;
  /** Toggle the "Loading…" dropdown state. */
  setLoading(flag: boolean): void;
  destroy(): void;
}

interface BlockrSelectSingleHandle extends BlockrSelectHandleBase {
  getValue(): string;
}

interface BlockrSelectMultiHandle extends BlockrSelectHandleBase {
  /** Copy of the selected values, in tag order. */
  getValue(): string[];
}

interface BlockrSelectStatic {
  single(
    container: HTMLElement,
    config: BlockrSelectSingleConfig
  ): BlockrSelectSingleHandle;
  multi(
    container: HTMLElement,
    config: BlockrSelectMultiConfig
  ): BlockrSelectMultiHandle;
}

/* --- Blockr.Input (blockr-input.js) --- */

interface BlockrInputConfig {
  /** Initial field value. */
  value?: string;
  /** Column names offered as completions (backticked when non-syntactic). */
  columns?: string[];
  /**
   * Function completions: category label (shown as meta) -> function names.
   * Functions insert with trailing "()", cursor between the parens.
   */
  categories?: Record<string, string[]>;
  placeholder?: string;
  /** Render a <textarea> instead of <input>; disables Enter -> onConfirm. */
  multiline?: boolean;
  /** Fires on every edit and on completion insert (no arguments). */
  onChange?: () => void;
  /**
   * Single-line only: fires with the trimmed value on Enter while the
   * completion popup is closed.
   */
  onConfirm?: (value: string) => void;
}

interface BlockrInputHandle {
  /** Root element (already appended to the container). */
  el: HTMLDivElement;
  /** Trimmed field value. */
  getValue(): string;
  setValue(v: string | null | undefined): void;
  /** Replace the column completions (function categories are fixed). */
  setColumns(cols: string[] | null | undefined): void;
  focus(): void;
  destroy(): void;
}

interface BlockrInputStatic {
  create(container: HTMLElement, config: BlockrInputConfig): BlockrInputHandle;
}

/* --- Block class / binding contract (JS: Blockr.registerBlock) --- */

/**
 * What every block class must implement. setState() must never fire the
 * callback; getValue() returns null until the first user submit.
 */
interface BlockrBlock {
  getValue(): unknown;
  setState(state: unknown): void;
  _callback: ((value: boolean) => void) | null;
}

/** Block container elements carry their instance as an expando. */
interface BlockrBlockHost extends HTMLElement {
  _block?: BlockrBlock;
}

/** Custom messages are dispatched by element id. */
interface BlockrMessage {
  id: string;
  [field: string]: unknown;
}

interface BlockrRegisterConfig {
  /** kebab-case name: binding `blockr.<name>`, container `<name>-block-container` */
  name: string;
  Block: new (el: HTMLElement) => BlockrBlock;
  messages?: Record<string, (block: any, msg: any) => void>;
}

interface BlockrPendingQueue {
  t: number;
  fns: Array<(block: BlockrBlock | undefined) => void>;
}

interface BlockrNamespace {
  uid(prefix?: string): string;
  escapeHtml(s: string): string;
  removeNode(node: Node | null | undefined): void;
  icons: Record<string, string>;
  onDocClick(el: Element, cb: (e: MouseEvent) => void): void;
  registerBlock(config: BlockrRegisterConfig): void;
  _docClick: Set<{ el: Element; cb: (e: MouseEvent) => void }>;
  _pending: Map<string, BlockrPendingQueue>;
  _enqueue(id: string, fn: (block: BlockrBlock | undefined) => void): void;
  _replayPending(el: BlockrBlockHost): void;
  /** Shared components (blockr-select.js / blockr-input.js) */
  Select?: BlockrSelectStatic;
  Input?: BlockrInputStatic;
}

declare var Blockr: BlockrNamespace;

interface Window {
  Blockr: BlockrNamespace;
}

/* --- Ambient Shiny / jQuery (no @types dependency) --- */

declare const Shiny: {
  InputBinding: new () => object;
  inputBindings: { register(binding: object, name: string): void };
  addCustomMessageHandler(
    name: string,
    handler: (msg: BlockrMessage) => void
  ): void;
  setInputValue(
    name: string,
    value: unknown,
    opts?: { priority?: 'event' | 'immediate' | 'deferred' }
  ): void;
};

declare const $: (selector: unknown) => {
  find(selector: string): unknown;
};
