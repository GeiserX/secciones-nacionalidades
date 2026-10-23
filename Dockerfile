# Self-hosted copy of the site. The public instance is https://nacionalidades.geiser.cloud
# The build stage makes plain static files, so it runs on the builder's own platform:
# under QEMU for arm64 it took more than 35 minutes.
FROM --platform=$BUILDPLATFORM node:22-alpine AS build
WORKDIR /app
COPY package.json package-lock.json ./
RUN npm ci
COPY index.html vite.config.js ./
COPY src src
COPY public public
RUN npm run build

FROM nginxinc/nginx-unprivileged:1.29-alpine
COPY --from=build /app/dist /usr/share/nginx/html
EXPOSE 8080
HEALTHCHECK CMD wget -q -O /dev/null http://localhost:8080/ || exit 1
