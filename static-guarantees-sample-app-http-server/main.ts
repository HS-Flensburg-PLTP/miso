import http from "node:http";

const port = 3456;
const waitTime = 1000;

const wait = (duration: number) =>
  new Promise<void>((resolve) => setTimeout(() => resolve(), duration));

const server = http.createServer(async (req, res) => {
  console.log("waiting for " + waitTime + "ms");
  await wait(waitTime);
  console.log("waited for " + waitTime + "ms");

  if (req.url.includes("random-attr")) {
    res.end(String(Math.floor(Math.random() * 8) + 1));
  } else {
    res.end();
  }
});

server.listen(port, () => console.log("server listening on port " + port));
