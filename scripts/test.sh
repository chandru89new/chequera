#!/bin/sh -e

# set vars
if [ -z "${PLUGIN}" ]; then
  echo "Error: PLUGIN env var not set. Example: PLUGIN=aws"
  exit 1
fi

if [ -z "${PLUGIN_REPO}" ]; then
  echo "Error: PLUGIN_REPO env var not set. Example: PLUGIN_REPO=github.com/turbot/steampipe-plugin-aws"
  exit 1
fi

PLUGIN_REPO=$(echo "${PLUGIN_REPO}" | sed -e 's/^https:\/\///')

# install plugin
echo "Installing plugin..."
steampipe plugin install $PLUGIN

# update plugin
echo "Updating plugin..."
steampipe plugin update $PLUGIN

# clone the repo
echo "Cloning repo..."
git clone https://$PLUGIN_REPO.git

PLUGIN_SLUG=$(echo "${PLUGIN_REPO}" | sed -e 's/.*\///')

# run chequera
echo "Running chequera..."
echo $PLUGIN_SLUG
chequera test --path ./steampipe-plugin-$PLUGIN/docs
