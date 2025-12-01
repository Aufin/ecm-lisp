import { Collection as Nervous } from 'spinalize'

export const Collection = Nervous.extend({
	parse(response) {
		return response.results
	}
})

export default Collection

										 
