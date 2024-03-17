
List of keys that I can used to 
extend/override the defaults for plugin config:
- cmd
- event
- ft
- keys
- opts
- dependencies

any other properties you use, will overide that property totally
meaning, say there is a property called 'conf' and you write the 
conf in your table then lazyvim will replace 'conf' property of the 
plugin entirely with what you wrote in your table

**NOTE**: Best way to make sure everything works is to run `:checkhealth`

