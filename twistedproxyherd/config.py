# Google Places API key
API_KEY="AIzaSyBFiGIho6fdev03Z-HiMsdMuXveVpwRygs"

# TCP port numbers for server instances
# Please use the port numbers allocated by the TA.
PORT_NUM = {
	'Alford': 12720,
	'Ball': 12721,
	'Hamilton': 12722,
	'Holiday': 12723,
	'Welsh': 12724
}

TALKS_TO = {
	"Alford" : ["Hamilton", "Welsh"],
	"Ball" : ["Holiday", "Welsh"],
	"Hamilton" : ["Alford", "Holiday"],
	"Holiday" : ["Ball", "Hamilton"],
	"Welsh" : ["Alford", "Ball"]
}
