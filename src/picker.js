// A searchable checkbox list for picking several items (municipalities, provinces).

export class Picker {
  /**
   * @param {HTMLElement} root
   * @param {{max?: number, placeholder?: string, onChange: (codes: string[]) => void}} options
   */
  constructor(root, { max = Infinity, placeholder = 'Buscar…', onChange }) {
    this.max = max;
    this.onChange = onChange;
    this.items = [];
    this.selected = [];
    root.innerHTML = '';
    this.search = Object.assign(document.createElement('input'), { type: 'search', placeholder });
    this.search.setAttribute('aria-label', placeholder);
    this.chips = document.createElement('div');
    this.chips.className = 'chips';
    this.list = document.createElement('ul');
    root.append(this.chips, this.search, this.list);
    this.search.addEventListener('input', () => this.renderList());
    this.list.addEventListener('change', (e) => {
      const code = e.target.value;
      if (e.target.checked) {
        if (this.selected.length >= this.max) {
          e.target.checked = false;
          return;
        }
        this.selected.push(code);
      } else this.selected = this.selected.filter((c) => c !== code);
      this.render();
      this.onChange(this.selected.slice());
    });
    this.chips.addEventListener('click', (e) => {
      const code = e.target.closest('button')?.dataset.code;
      if (!code) return;
      this.selected = this.selected.filter((c) => c !== code);
      this.render();
      this.onChange(this.selected.slice());
    });
  }

  /** items: [{code, name}] */
  setItems(items, selected = []) {
    this.items = items;
    const known = new Set(items.map((i) => i.code));
    this.selected = selected.filter((c) => known.has(c)).slice(0, this.max);
    this.search.value = '';
    this.render();
  }

  setSelected(codes) {
    this.selected = codes.slice(0, this.max);
    this.render();
  }

  render() {
    const names = new Map(this.items.map((i) => [i.code, i.name]));
    this.chips.innerHTML = '';
    for (const code of this.selected) {
      const b = document.createElement('button');
      b.type = 'button';
      b.dataset.code = code;
      b.textContent = `${names.get(code) ?? code} ×`;
      b.setAttribute('aria-label', `Quitar ${names.get(code) ?? code}`);
      this.chips.append(b);
    }
    this.renderList();
  }

  renderList() {
    const q = normalize(this.search.value);
    const chosen = new Set(this.selected);
    const frag = document.createDocumentFragment();
    for (const item of this.items) {
      if (q && !normalize(item.name).includes(q)) continue;
      const li = document.createElement('li');
      const label = document.createElement('label');
      const box = Object.assign(document.createElement('input'), { type: 'checkbox', value: item.code, checked: chosen.has(item.code) });
      label.append(box, ' ', item.name);
      li.append(label);
      frag.append(li);
    }
    this.list.replaceChildren(frag);
  }
}

export const normalize = (s) => s.normalize('NFD').replace(/\p{Diacritic}/gu, '').toLowerCase();
