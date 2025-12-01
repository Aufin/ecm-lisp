import { Policy } from './model'
import { Policies } from './collection'
import { Autocomplete, View, M, $, _, Backbone } from 'spinalize'

const atScrollTop = (el, offset = 5) => {
	// Or near scroll top ...
	el.scrollTop < offset
}

const atScrollBottom = (el, offset = 5) => {
	// offset is "how many px away from the end?"
	// this is light enough for the scroll event.

	const pos = (el.scrollHeight - el.offsetHeight) - el.scrollTop

	return pos < offset;
}
	
	
export const SelectPolicy = Autocomplete.extend({
	label: "Policy",
	term: '',
	limit: 10,
	offset: 0,
	count: 0,
	addMenuItem(item) {
		const ma = this.instance,
			  ul = this.dropdown.dropdownEl,
			  li = ma._createDropdownItem(item)
		ma.menuItems.push(item)
		ul.append(li)
		return li
	},
	addModel(model) {
		return this.addMenuItem({
			id: model.id,
			text: model.toString()
		})
	},
	onScroll (e) {
		const ul = e.currentTarget;
		if (atScrollBottom(ul)) {
			const offset = this.offset + this.limit
			if (offset <= this.count) {
				const li = ! this.loadingLi && this.createLoadingLi()
				if (li) {
					this.loadingLi = li
					this.dropdown.dropdownEl.append(li)
					const addr =  (model) => {
						this.addModel(model);
					}

					
					this.collection.on('add', addr, this)
					this.search(this.term, this.limit, offset)
						.then(_ => {
							this.collection.off('add', addr, this)
							setTimeout(() => { li.remove() }, 500)
							this.loadingLi = false
						})
					
				}
			}
		}
	},
	onSearch(term, el) {
		// console.warn("onSearch", term)
		this.term = term
		this.collection.reset([])
		this.search(term, this.limit, 0)
			.then(r => {
				this.clearData()
				this.setMenuFromCollection()
				// console.warn('data?', this.data.length, r, this.instance.menuItems)
				if (this.data.length > 0 ) {
					this.triggerSomeResults()
				} else {
				//	console.warn('TRigger none', this.triggerNoResults)
					this.triggerNoResults()
				}
				return r
			})
	},
	createLoadingLi() {
		const li = document.createElement('li'),
			  n = this.limit + this.offset,
			  total = this.count,
			  txt = `(${n} of ${total}) loading ...`

		li.innerHTML = txt + `<div class="progress"
          ><div class="indeterminate"></div></div>`
		return li
		
		
	},
	search(term, limit, offset) {
		return this.collection.search(term, limit, offset)
			.then(r => {
				this.limit = r.limit;
				this.count = r.count;
				this.offset = r.offset || 0;
				return r
			})
		
	},
	gotoCreate() {
		window.location.assign('/ecm/create?create[type]=policy&access[read-only]=false')
	},
	initialize(...args) {
		Autocomplete.prototype.initialize.call(this, ...args)

		// NB: Why is this shared? I think it's because the default instance.option's?
		// this.instance._data = []

		this.dropdown.dropdownEl.addEventListener(
			"scroll", (e) => {
				this.onScroll(e)
			}
		)
		this.createButton.addEventListener('click', _ => { this.gotoCreate()})
		
		if (args[0].limit) { this.limit = args[0].limit }
		if (this.collection) {
			this.setMenuFromCollection()
		} else {
			const ps = new Policies()
			ps.search(this.term, this.limit).then(s => {
				this.collection = ps;
				this.setMenuFromCollection()
				this.limit = s.limit;
				this.count = s.count;
				this.offset = s.offset || 0;
			})
		}

		this.instance.options.onSearch = (...args) => {
			return this.onSearch(...args)
		}
		// globalThis.SelP = this
	}
})




