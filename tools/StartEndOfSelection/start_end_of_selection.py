import sublime, sublime_plugin

class start_end_of_selection(sublime_plugin.WindowCommand):
	def run(self):
		print('selection_plugin__ called')
		window = self.window
		view = window.active_view()
		sel = view.sel()

		region1 = sel[0]
		start = view.rowcol(region1.begin())
		end = view.rowcol(region1.end())
		sublime.set_clipboard('%i, %i, %i, %i' % (start[0] + 1, start[1] + 1, end[0] + 1, end[1] + 1))
