#!/bin/bash
set -e

VPS_IP="72.61.177.23"
VPS_USER="deploy"

echo "🚀 Deploying Shiny app to VPS..."

echo "📤 Pushing to GitHub..."
git add .
git commit -m "Deploy $(date '+%Y-%m-%d %H:%M:%S')" || echo "No changes to commit"
git push origin main

echo "📥 Pulling on VPS and restarting..."
ssh ${VPS_USER}@${VPS_IP} << 'ENDSSH'
cd /srv/shiny-server/
sudo git fetch origin
sudo git reset --hard origin/main
sudo chown -R shiny:shiny /srv/shiny-server/
sudo chmod -R 755 /srv/shiny-server/
sudo systemctl restart shiny-server
echo "✅ Shiny app deployed!"
ENDSSH

echo ""
echo "✅ Deploy complete! Visit https://shiny.femnollnoll.cloud"
