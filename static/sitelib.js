/* sitelib.js --- Library of javascript functions for IRClogs.
 *
 * Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>
 *
 * Free software, licensed under the GNU GPL v3 (or later), 
 * see <http://www.gnu.org/licenses/>.
 */

function activate_log_options() {
    $('#options input[@name=opt-btn]').hide();
    $('#options input[@name=events]').click(apply_log_options);
    apply_log_options();
}

function apply_log_options() {
    events = $('#options input[@name=events]').attr("checked");
    $("table.log tr").each(function(i) {
	tr = $(this);
	if (tr.find("td").hasClass("event")) {
	    tr.css({ display : (events ? '' : 'none') });
	}
    });
}
