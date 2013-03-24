/* Copyright (C) 2013  Tom Willemsen <tom at ryuslash dot org>

   This file is part of CLark

   CLark is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   CLark is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with CLark. If not, see <http://www.gnu.org/licenses/>.
 */
/// Commentary:

// A wrapper script that should allow conkeror to interface with
// clark.  Does not yet allow searching through collected bookmarks,
// only adding new ones.

/// Code:

define_variable("clark_program", "clark",
                "The location of the clark executable.");

define_browser_object_class(
    "clark-bookmark", null,
    function (I, prompt) {
        check_buffer(I.buffer, content_buffer);
        var result = yield I.buffer.window.minibuffer.read(
            $prompt = prompt, $completer = clark_complete
        );
        yield co_return(result);
    }
);

function clark_add_url(I, url, title)
{   // Add URL to clark, ask for a title (provide TITLE as a
    // default), description and any number of tags.
    let url_string = load_spec_uri_string(load_spec(url));
    let title = yield I.minibuffer.read($prompt="name (required): ",
                                        $initial_value=title);
    let description = yield I.minibuffer.read(
        $prompt="extended description: "
    );
    let tags = yield I.minibuffer.read(
        $prompt="tags (comma delimited): "
    );
    let command = clark_program + ' add "' + url_string + '" "' + title
            + '" "' + description + '" \''
            + tags.split(',').map(function (str)
                                  { return str.trim(); }).join("' '")
            + "'";
    yield shell_command(command);
}

function clark_add(I) {
    check_buffer(I.buffer, content_buffer);
    let result = yield clark_add_url(I, I.buffer.top_frame,
                                        I.buffer.title);

    if (!result)
        I.window.minibuffer.message('Added to clark');
    else
        I.window.minibuffer.message('Couldn\'t add to clark');
}
interactive("clark-add",
            "Bookmark the current page in clark",
            clark_add);

function clark_add_link(I) {
    check_buffer(I.buffer, content_buffer);
    bo = yield read_browser_object(I);
    let result = yield clark_add_url(I, encodeURIComponent(bo),
                                        bo.textContent);

    if (!result)
        I.window.minibuffer.message('Added to clark');
    else
        I.window.minibuffer.message('Couldn\'t add to clark');
}
interactive("clark-add-link",
            "Select and bookmark a link in clark",
            clark_add_link);

function clark_complete(input, pos, conservative)
{
    if (pos == 0 && conservative)
        yield co_return(undefined);

    let str = input.substring(0, pos);

    var data = "", error = "", ret = [];
    var result = 0;

    if (str == "")
        result = yield shell_command(
            clark_program + " --script",
            $fds = [{ output: async_binary_string_writer("") },
                    { input: async_binary_reader(function (s) data += s || "") },
                    { input: async_binary_reader(function (s) error += s || "") }]);
    else
        result = yield shell_command_with_argument(
            clark_program + " --script search {}", str,
            $fds = [{ output: async_binary_string_writer("") },
                    { input: async_binary_reader(function (s) data += s || "") },
                    { input: async_binary_reader(function (s) error += s || "") }]);

    if (result != 0 || error != "")
        throw new Error("result: " + result + ", error: " + error);
    else if (data != "") {
        data.split('').forEach(function (row) {
            ret.push(row.split(''));
        });

        let c = { count: ret.length,
                  get_string: function (i) ret[i][0],
                  get_description: function (i) ret[i][1],
                  get_input_state: function (i) [ret[i][2]] };
        yield co_return(c);
    }
}

interactive("clark-find-url",
            "Find a page from clark in the current buffer",
            "find-url",
            $browser_object = browser_object_clark_bookmark);
interactive("clark-find-url-new-buffer",
            "Find a page from clark in a new buffer",
            "find-url-new-buffer",
            $browser_object = browser_object_clark_bookmark);


provide("clark");
