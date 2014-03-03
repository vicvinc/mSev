#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/event.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define PORT "8080"

static char *foo = 

"HTTP/1.1 200 OK\r\n"
"Server: httpd\r\n"
"Connection: close\r\n"
"Content-Type: text/plain\r\n\r\n"
"Hello,World";

void *get_in_addr(struct sockaddr *sa)
{
	if (sa->sa_family == AF_INET) 
		return &(((struct sockaddr_in *)sa)->sin_addr);
	return &(((struct sockaddr_in6 *)sa)->sin6_addr);\
}

#define LISTEN_QUEUE   10

int main(void)
{
	struct kevent chlist, evlist[LISTEN_QUEUE];
	int kq, nev;

	int listenfd;
	int newfd;
	struct sockaddr_storage remoteaddr;
	socklen_t addrlen;

	char buf[1024];
	int nbytes;

	char remoteIP[INET6_ADDRSTRLEN];

	int yes = 1;
	int i, j, rv;

	struct addrinfo hints, *ai, *p;
	bzero(&hints, sizeof(hints));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;

	if ((rv = getaddrinfo(NULL, PORT, &hints, &ai)) != 0) {
		fprintf(stderr, "kqueuehttpd: %s\n", gai_strerror(rv));
		exit(1);
	}

	for (p = ai; p != NULL; p = p->ai_next) {
		listenfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (listenfd < 0) 
			continue;
		setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int));

		if (bind(listenfd, p->ai_addr, p->ai_addrlen) < 0) {
			close(listenfd);
			continue;
		}
		break;
	}

	if (p == NULL) {
		fprintf(stderr, "kqueuehttpd failure");
		exit(2);
	}

	freeaddrinfo(ai);

	if (listen(listenfd, 10) == -1) {
		perror("listen");
		exit(3);
	}

	if ((kq = kqueue()) == -1) {
		perror("kqueue");
		exit(1);
	}
	EV_SET(&chlist, listenfd, EVFILT_READ, EV_ADD, 0, 0, 0);
	kevent(kq, &chlist, 1, NULL, 0, NULL);

	while (1) {
		nev = kevent(kq, NULL, 0, evlist, LISTEN_QUEUE, NULL);
		for (i = 0; i < nev; i++) {
			if (evlist[i].ident == listenfd) {
				newfd = accept(listenfd, (struct sockaddr *)&remoteaddr,
					&addrlen);
				if (newfd == -1) {
					perror("accept");
				} else {
					EV_SET(&chlist, newfd, EVFILT_READ, EV_ADD, 0, 0, 0);
					kevent(kq, &chlist, 1, NULL, 0, NULL);
					printf("kqueueserver: new connection from %s on "
						"socket %d\n", 
						inet_ntop(remoteaddr.ss_family,
							get_in_addr((struct sockaddr *)&remoteaddr),
							remoteIP, INET6_ADDRSTRLEN),
							newfd);
				}
			} else if (evlist[i].flags & EV_EOF) {
				close(evlist[i].ident);
			} else {
				if ((nbytes = recv(evlist[i].ident, buf, sizeof(buf), 0)) <= 0) {
					if (nbytes == 0) {
						printf("kqueuehttpd: socket %d hung up\n", i);
					} else {
						perror("recv");
					}
					close(i);
				} else {
					if (send(evlist[i].ident, foo, strlen(foo), 0) == -1) {
						perror("send");
					}
					close(evlist[i].ident);
				}
			}
		}
	}
	return 0;
}

