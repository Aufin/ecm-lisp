import {
	Autocomplete,
	autoInit,
	Select,
	View,
	Widget,
	Modal,
	M, $, _,
	Backbone
} from 'spinalize'

import { RiskTypes, RiskCodes } from './collection'
import { SelectPolicy } from '../policy/view'
import { SelectContract } from '../contract/view'
import { Risk } from './model'

import editableHtml from './editable.html'

const supportingTextHtml = '<span style="opacity: 0" class="supporting-text">click for items and scroll or type to search</span>'

const getSupportingTextEl = function () {
	const sel = this.el.querySelector(".supporting-text")
	if (sel) {
		return sel
	} else {
		const div = document.createElement('div')
		div.innerHTML = supportingTextHtml;
		const sel = div.firstElementChild
		this.el.append(sel)
		return sel
	}
}

const getSupportingTextHtml = function () {
	const html = this._supportingTextHtml;
	if (html) {
		return html
	} else {
		const sel = getSupportingTextEl.call(this),
			  html = sel.innerHTML
		this._supportingTextHtml = html
		return html
	}
}
		

const addSupportingTextHover = function (trigger) {
	const supText = getSupportingTextEl.call(this)
	trigger.onmouseover = _ => {
		this.selectedValues.length === 0 ? supText.style.opacity = 1 : null
	}
		
	trigger.onmouseout = _ => {
		this.selectedValues.length === 0 ? supText.style.opacity = 0 : null
	}
}

const addErrorHandler = function () {
	this.model.on("invalid", (model, error) => {
		const err = error.find(e =>
			e.attribute == this.attribute)
		if (err) {
			const supEl = getSupportingTextEl.call(this),
				  supText = getSupportingTextHtml.call(this)
			supEl.innerHTML = err.error
			supEl.style.opacity = 1;
			supEl.style.color = 'red';
			// console.error('caught invalid on', error,  model, supText)
		}
	})

	this.model.on('change:'+this.attribute, () => {
		const supEl = getSupportingTextEl.call(this),
			  supText = getSupportingTextHtml.call(this)
		supEl.innerHTML = supText
		supEl.style.color = '';
	})
}



export const EditRisk = Widget.extend({
	model: false,
	hideModal() {
		this.popover.hide()
	},
	initialize() {
		Widget.prototype.initialize.call(this)
		this.$el.html(editableHtml)
		
		this.popover = new Modal({
			el: this.el,
			header: "<h4>Error</h4>",
			footer: `<button data-popok tabindex="0" class="btn outlined waves-effect" style="color:var(--md-sys-color-on-background);">Ok</button>`
		})

		this.delegateEvents({ 'click [data-popok]': 'hideModal'})

		this.model.on('error', function(model_or_collection, xhr, options) {
			this.popover.set('content', xhr.response)
			this.popover.el.querySelectorAll('div').forEach(div => {
			 	div.style.backgroundColor = 'var(--md-sys-color-error-container)'
			})
			
			this.popover.show()
		}, this)
		
		if (!this.model) {
			this.model = new Risk()
		}
		autoInit({
			el: this.el,
			env: {
				SelectRiskPolicy,
				SelectRiskContract,
				SelectRiskType,
				SelectRiskCode,
				SubmitRisk
			},
			options: { model: this.model }
			
			
		})
	}
})
	

export const SubmitRisk = View.extend({
	initialize() {
		// this.model.on('request', _ => {
		// 	this.el.classList.add("disabled")
		// })
		this.$el.on('click', _ => {
			// console.debug('Clicked! cache', _ )
			if (this.model.isValid()) {
				const overlay = document.querySelector('.risk-editable [data-overlay]')
				console.debug('Why overlay no?', overlay, this.el)
				if (!this.prevHtml) { this.prevHtml = this.el.innerHTML }
				this.el.innerHTML = "Saving ..."
				

				overlay.style.display = 'flex'
				this.model.save({}, {
					success: (model, response, options) => {
						//console.debug("Ok, this is saved", model, response, options)
						// Simulate an HTTP redirect:
						window.location.replace(`/ecm/risk/${model.id}`);
						this.el.innerHTML = this.prevHtml;
						overlay.style.display = 'none'
					},
					error: (model, response, options) => {
						this.el.innerHTML = this.prevHtml;
						overlay.style.display = 'none'
						console.error('Risk save error:', response, model, options, this)
					}
				})
					
			}
									  
				
		})
	},
	prevHtml: false,
})

export const SelectRiskPolicy = SelectPolicy.extend({
	placeholder: "(required)",
	label: "Policy",
	attribute: "policy_id",
	initialize() {
		SelectPolicy.prototype.initialize.call(this, {})
		addSupportingTextHover.call(this, this.el)
		addErrorHandler.call(this)
	}

})

export const SelectRiskContract = SelectContract.extend({
	placeholder: "(required)",
	label: "Contract",
	attribute: "contract_id",
	initialize() {
		SelectContract.prototype.initialize.call(this, {})
		addSupportingTextHover.call(this, this.el)
		addErrorHandler.call(this)
	}


})

export const SelectRiskType = Select.extend({
	label: "Risk Type",
	attribute: "risk_type",
	initialize() {
		this.collection = new RiskTypes()
		this.collection.fetch({}).then(_ => {
			const opts = this.collection.map(type =>
				`<option value="${type.id}">${type.id}</option>`)
			this.el.innerHTML = opts.join('\n')
			Select.prototype.initialize.call(this, {})
		})
	}
})

export const SelectRiskCode = Autocomplete.extend({
	label: "Risk Code",
	attribute: "risk_code",
	placeholder: "(required)",
	initialize(...args) {
		Autocomplete.prototype.initialize.call(this, ...args)

		addSupportingTextHover.call(this, this.el)
		addErrorHandler.call(this)
	
		if (this.collection) {
			this.loadMenu()
		} else {
			const codes = new RiskCodes()
			this.collection = codes;
			codes.fetch({}).then(_ => {
				this.loadMenu()
			})
		}
	},
	loadMenu() {
		const data = this.collection.map(c => {
			return {
				id: c.id,
				text: ''+c.id+' : '+c.get('description')
			}
		})

		// console.debug('data from collection', data)
		this.setMenuItems(data, null, false)
		this.instance.options.minLength = 0
	}
})









	
