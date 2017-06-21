import sys, time, config, json 
from twisted.python import log
from twisted.internet.protocol import Protocol, ServerFactory, ClientFactory
from twisted.protocols.basic import LineReceiver
from twisted.web.client import getPage
from twisted.internet import reactor, defer

class ServerHerdProtocol(LineReceiver):

	def connectionMade(self):
		log.msg('Client connection from {}'.format(self.transport.getPeer()))

	def connectionLost(self, reason):
		log.msg('Client connection lost from server. Reason:', reason)

	def connectionFailed(self, reason):
		log.msg('Connection failed. Reason:', reason)

	def lineReceived(self, line):
		log.msg("Received message: {}".format(line))
		now = time.time()

		if line == "":
			return

		rcvd = line.split()

		if rcvd[0] == "AT":
			self.AT_handler(line)
		elif rcvd[0] == "IAMAT":
			self.IAMAT_handler(line, now)
		elif rcvd[0] == "WHATSAT":
			self.WHATSAT_handler(line)
		else:
			log.msg("Sent response: {}".format(" ".join(["?", line])))
			self.sendLine(" ".join(["?", line]))
	
	def AT_handler(self, line):

		rcvd = line.split()
		name = rcvd[3]
		timestamp = float(rcvd[5])
		location = rcvd[4]

		"""If timestamp is less, do not update"""
		if name in self.factory.clients:
			if timestamp < self.factory.clients[name]['time']:
				log.msg("Time travel, no update.")
				return
			elif timestamp == self.factory.clients[name]['time']:
				log.msg("Same message, no update.")
				return

		self.factory.clients[name] = {"AT": line, "time": timestamp, "location": location}

		"""Flood to my neighbors."""
		self.flood(line)

	def IAMAT_handler(self, line, time):
		rcvd = line.split()

		if len(rcvd) != 4:
			log.msg("Sent response: {}".format(" ".join(["?", line])))
			self.sendLine(" ".join(["?", line]))
			return

		name = rcvd[1]
		timestamp = float(rcvd[3])

		"""If timestamp is less, do not update"""
		if name in self.factory.clients:
			if timestamp < self.factory.clients[name]['time']:
				log.msg("Time travel, no update.")
				return
		
		difference = time - timestamp
		time_rcvd = "{0:+.06f}".format(difference) if difference >= 0  else "{0:-.06f}".format(difference)
		response = " ".join(["AT", self.factory.name, time_rcvd, line.split(" ", 1)[1]])
		self.factory.clients[name] = {"AT": response, "time": timestamp, "location": rcvd[2]}

		"""Flood to my neighbors."""
		self.flood(response)

		log.msg("Sent response: {}".format(response))
		self.sendLine(response)

	def jdata_parse(self, page, name, upper_bound):
		jdata = json.loads(page)

		results = jdata["results"]
		log.msg(upper_bound)
		jdata["results"] = results[0:int(upper_bound)]
		return_json = json.dumps(jdata, indent=2)
		return_json = return_json.replace("\n\n", "\n")
		return_json = return_json.strip("\n")

		response = "{0}\n{1}\n\n".format(self.factory.clients[name]["AT"],return_json)
		log.msg("Sending response: \n{}".format(response))
		self.sendLine(response)


	def WHATSAT_handler(self, line):
		rcvd = line.split()

		if len(rcvd) != 4:
			log.msg("Sent response: {}".format(" ".join(["?", line])))
			self.sendLine(" ".join(["?", line]))
			return

		name = rcvd[1]
		radius = rcvd[2]
		places = rcvd[3]

		if not name in self.factory.clients:
			self.sendLine("{} doesn't exist in our database.".format(name))
			return
		if int(radius) > 50:
			self.sendLine("Please specify a radius that is less than 50km.")
			return
		if int(places) > 20:
			self.sendLine("Please request at maximum 20 nearby locations.")
			return

		location = self.factory.clients[name]['location']
		split_at = 1
                for item in location[1:]:
        	        if item != '+' and item != '-':
        		        split_at += 1
        	        else:
        		        break
                coords = "".join([location[0:split_at], ",", location[split_at:]])
                key = config.API_KEY
                places_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={0}&radius={1}&key={2}".format(coords, radius, key)
                places_response = getPage(places_url)

                log.msg("Sending a request to {}".format(places_url))

                places_response.addCallback(self.jdata_parse, name=name, upper_bound=places)

		
	def flood(self, response):
		for neighbor in config.TALKS_TO[self.factory.name]:
			log.msg(self.factory.name + " is forwarding this info to " + neighbor)
			connect = reactor.connectTCP('localhost', config.PORT_NUM[neighbor], PropagateMessageFactory(self.factory.name, response))


class ServerHerdFactory(ServerFactory):

	protocol = ServerHerdProtocol

	def __init__(self, name):
		self.name = name
		self.clients = {}

"""The server sending the flood message becomes a client to its neighbors"""
class PropagateMessageProtocol(LineReceiver):

        def __init__(self, factory):
                self.factory = factory

        def connectionMade(self):
                log.msg("Connection made. Sending to server at .".format(self.transport.getPeer()))
                log.msg(self.factory.data)
                self.sendLine(self.factory.data)
                self.transport.loseConnection()
                log.msg("Finished sending the message, closing connection.")

class PropagateMessageFactory(ClientFactory):

        def __init__(self, name, data):
                log.msg("Initializing myself, {}, as a client.".format(name))
                self.name = name
                self.data = data

        def buildProtocol(self, addr):
                return PropagateMessageProtocol(self)

        def clientConnectionLost(self, connector, reason):
                log.msg('Lost connection to server.  Reason:', reason)

        def clientConnectionFailed(self, connector, reason):
                log.msg('Connection failed. Reason:', reason)

def main():

	server = sys.argv[1]
	log.startLogging(open('{}.log'.format(server), 'w'), setStdout=False)
	if server not in config.PORT_NUM:
		sys.exit("Please specify port Alford, Ball, Hamilton, Holiday, or Welch.")
	else:
		port = config.PORT_NUM[server]

	reactor.listenTCP(port, ServerHerdFactory(server))
	reactor.run()

if __name__ == '__main__':
	main()
