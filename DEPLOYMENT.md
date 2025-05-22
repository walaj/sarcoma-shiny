# Deploying the Sarcoma Shiny App on Google Cloud VM

This guide provides instructions for deploying the Sarcoma Shiny app on a Google Cloud Platform (GCP) virtual machine (VM) so it can be accessed by users without requiring them to upload any data.

## Prerequisites

- A Google Cloud Platform account
- Basic knowledge of the Linux command line
- The Sarcoma Shiny app code and data files

## 1. Setting Up a Google Cloud VM Instance

1. Navigate to the Google Cloud Console: https://console.cloud.google.com/
2. Create a new project or select an existing one
3. Go to Compute Engine > VM Instances
4. Click "Create Instance"
5. Configure your VM:
   - Name: Choose a descriptive name (e.g., "sarcoma-shiny-server")
   - Region and Zone: Choose based on where your users are located
   - Machine configuration: 
     - For this app, select at least 2 vCPUs and 8 GB memory (e7-standard-2)
     - You can start with less and scale up if needed
   - Boot disk: 
     - Ubuntu 20.04 LTS
     - Size: At least 20GB (depending on your data size)
   - Firewall: Check "Allow HTTP traffic" and "Allow HTTPS traffic"
6. Click "Create"

## 2. Connecting to Your VM

Once the VM is created, you can connect to it via SSH by:

1. Clicking the SSH button in the Google Cloud Console
2. Or, if you prefer using a terminal, using the gcloud command:
   ```
   gcloud compute ssh sarcoma-shiny-server --zone=your-zone
   ```

## 3. Installing R and Required Packages

Update the system and install R:

```bash
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo apt-get update
sudo apt-get install -y r-base r-base-dev
```

Install system dependencies for R packages:

```bash
sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev
sudo apt-get install -y libgdal-dev libproj-dev libgeos-dev libudunits2-dev
```

Install required R packages:

```bash
sudo R -e "install.packages(c('shiny', 'ggplot2', 'plotly', 'data.table', 'GenomicRanges'), repos='https://cloud.r-project.org/')"
```

## 4. Installing Shiny Server

```bash
# Install gdebi for easier package installation
sudo apt-get install -y gdebi-core

# Download the latest version of Shiny Server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb

# Install Shiny Server
sudo gdebi shiny-server-1.5.20.1002-amd64.deb
```

## 5. Deploying the Sarcoma Shiny App

1. Create a directory for your app in the Shiny Server apps folder:

```bash
sudo mkdir -p /srv/shiny-server/sarcoma-app
```

2. Copy your app files to the server. You can use one of the following methods:

   - **scp command** (from your local machine):
     ```bash
     # Assuming your app.R and data directory are in a folder called 'sarcoma-shiny'
     scp -r /path/to/sarcoma-shiny/* your-username@external-ip:/tmp/sarcoma-app/
     ```

   - **gcloud copy files** (alternative to scp):
     ```bash
     gcloud compute scp --recurse /path/to/sarcoma-shiny/* sarcoma-shiny-server:/tmp/sarcoma-app/ --zone=your-zone
     ```

3. Move the files to the Shiny Server directory:

```bash
# On the VM
sudo cp -r /tmp/sarcoma-app/* /srv/shiny-server/sarcoma-app/
sudo chown -R shiny:shiny /srv/shiny-server/sarcoma-app/
```

## 6. Configuring Shiny Server

1. Edit the Shiny Server configuration file:

```bash
sudo nano /etc/shiny-server/shiny-server.conf
```

2. Add or modify the configuration to match your needs:

```
# Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838;

  # Define a location at the base URL
  location / {
    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;
    
    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;
    
    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
  
  # Define a specific location for our sarcoma app
  location /sarcoma {
    app_dir /srv/shiny-server/sarcoma-app;
    log_dir /var/log/shiny-server;
  }
}
```

3. Restart Shiny Server:

```bash
sudo systemctl restart shiny-server
```

## 7. Setting Up a Firewall Rule for Shiny Server

By default, the Shiny Server runs on port 3838. You need to allow external access to this port:

```bash
# Create a firewall rule to allow traffic on port 3838
gcloud compute firewall-rules create shiny-server --allow tcp:3838 --target-tags=shiny-server
gcloud compute instances add-tags sarcoma-shiny-server --tags=shiny-server --zone=your-zone
```

## 8. Accessing Your Shiny App

Your Shiny app should now be accessible at:

```
http://[EXTERNAL-IP]:3838/sarcoma/
```

Replace `[EXTERNAL-IP]` with your VM's external IP address, which you can find in the VM instances list in the Google Cloud Console.

## 9. Setting Up a Domain Name (Optional)

For a more professional appearance, you can set up a domain name:

1. Purchase a domain name from a domain registrar
2. Add an A record pointing to your VM's external IP address
3. Optionally, set up HTTPS using Let's Encrypt for secure connections

## 10. Setting Up HTTPS with Let's Encrypt (Optional)

For secure connections, you can set up HTTPS using Let's Encrypt:

```bash
# Install Certbot
sudo apt-get install -y certbot

# Get a certificate
sudo certbot certonly --standalone -d your-domain.com

# Set up a reverse proxy with Nginx
sudo apt-get install -y nginx
```

Configure Nginx to proxy requests to your Shiny Server:

```bash
sudo nano /etc/nginx/sites-available/sarcoma-app
```

Add the following configuration:

```
server {
    listen 80;
    server_name your-domain.com;
    return 301 https://$host$request_uri;
}

server {
    listen 443 ssl;
    server_name your-domain.com;

    ssl_certificate /etc/letsencrypt/live/your-domain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/your-domain.com/privkey.pem;

    location / {
        proxy_pass http://localhost:3838/sarcoma/;
        proxy_redirect http://localhost:3838/sarcoma/ /;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_read_timeout 600s;
        proxy_buffering off;
    }
}
```

Enable the site and restart Nginx:

```bash
sudo ln -s /etc/nginx/sites-available/sarcoma-app /etc/nginx/sites-enabled/
sudo systemctl restart nginx
```

## 11. Setting Up Automatic Updates (Optional)

To keep your app up-to-date, you can set up a script to pull the latest changes from your Git repository:

```bash
# Create an update script
sudo nano /srv/shiny-server/update-sarcoma-app.sh
```

Add the following content:

```bash
#!/bin/bash
cd /srv/shiny-server/sarcoma-app
git pull
sudo systemctl restart shiny-server
```

Make the script executable:

```bash
sudo chmod +x /srv/shiny-server/update-sarcoma-app.sh
```

Set up a cron job to run the script daily:

```bash
sudo crontab -e
```

Add the following line:

```
0 0 * * * /srv/shiny-server/update-sarcoma-app.sh >> /var/log/sarcoma-app-update.log 2>&1
```

## 12. Monitoring and Maintenance

1. Check the Shiny Server logs for errors:
   ```bash
   sudo tail -f /var/log/shiny-server/sarcoma-*
   ```

2. Monitor system resources:
   ```bash
   top
   htop  # Install with: sudo apt-get install htop
   ```

3. Back up your application regularly:
   ```bash
   # Example backup command
   sudo tar -czf /backup/sarcoma-app-$(date +%Y%m%d).tar.gz /srv/shiny-server/sarcoma-app/
   ```

## Troubleshooting

1. **App not loading:**
   - Check Shiny Server status: `sudo systemctl status shiny-server`
   - Check logs: `sudo tail -f /var/log/shiny-server/sarcoma-*`
   - Verify permissions: `sudo chown -R shiny:shiny /srv/shiny-server/sarcoma-app/`

2. **Performance issues:**
   - Consider upgrading your VM to a higher tier
   - Check for memory leaks in your app
   - Optimize data loading (use caching where possible)

3. **Security concerns:**
   - Keep your system updated: `sudo apt-get update && sudo apt-get upgrade -y`
   - Consider setting up a firewall: `sudo ufw enable`
   - Restrict SSH access to known IPs if possible

## Additional Resources

- [Shiny Server Administrator's Guide](https://docs.rstudio.com/shiny-server/)
- [Google Cloud VM Documentation](https://cloud.google.com/compute/docs)
- [Let's Encrypt Documentation](https://letsencrypt.org/docs/)