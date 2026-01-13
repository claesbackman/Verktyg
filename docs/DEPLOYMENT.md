# Deployment Guide

This guide will help you deploy your Shiny app to shinyapps.io and set up GitHub Pages for the documentation website.

## Part 1: Deploy to shinyapps.io

### Step 1: Create a shinyapps.io account

1. Go to [shinyapps.io](https://www.shinyapps.io/)
2. Click "Sign Up" and create a free account
3. The free tier includes:
   - 5 applications
   - 25 active hours per month
   - Perfect for personal projects and demos

### Step 2: Install rsconnect package

In R or RStudio, run:

```r
install.packages("rsconnect")
```

### Step 3: Configure rsconnect with your account

This step connects your local R installation to your shinyapps.io account. You only need to do this once.

1. Log into [shinyapps.io](https://www.shinyapps.io/) with your account
2. Click your name/avatar in the top right corner of the dashboard
3. Select **"Tokens"** from the dropdown menu
4. You'll see your account token. Click **"Show"** next to the token
5. Click **"Show Secret"** to reveal your secret key
6. Copy the entire code snippet that appears (it will look like this):

```r
rsconnect::setAccountInfo(name='YOUR-USERNAME',
                          token='YOUR-TOKEN',
                          secret='YOUR-SECRET')
```

7. Open R or RStudio
8. Paste the code snippet into the console and press Enter
9. You should see a confirmation message. This configuration is saved permanently on your computer.

**Note:** Replace `YOUR-USERNAME`, `YOUR-TOKEN`, and `YOUR-SECRET` with the actual values from your shinyapps.io account. The code snippet provided by shinyapps.io will already have these filled in.

### Step 4: Deploy your app

**IMPORTANT:** The app files are now in the `app/` folder.

**Recommended method (especially for Windows users with special characters in paths):**

In RStudio:

1. Open your project: File → Open Project → Select `Verktyg.Rproj`
2. Navigate to the `app/` folder in the Files pane (bottom right)
3. Open `app/ui.R` or `app/server.R` in the editor
4. Click the "Publish" button (blue icon in top right of editor)
5. Select "Publish Application"
6. Choose which files to include (select `ui.R`, `server.R`, `global.R`, and `www/` folder if you have images)
7. Give your app a name (e.g., "Verktyg")
8. Click "Publish"

**Alternative: Use the console** (if you get path encoding errors, use the GUI method above instead):

```r
library(rsconnect)
# Use relative path with forward slash to avoid encoding issues
setwd("./app")  # or use: setwd("app")
rsconnect::deployApp(appName = "Verktyg")
```

**If you encounter path errors**, try specifying the app directory explicitly:

```r
library(rsconnect)
# Get the full path to app folder (handles special characters better)
appDir <- normalizePath("./app", winslash = "/", mustWork = TRUE)
rsconnect::deployApp(appDir = appDir, appName = "Verktyg")
```

### Step 5: Note your app URL

After deployment, your app will be available at:
```
https://YOUR-USERNAME.shinyapps.io/Verktyg/
```

Save this URL - you'll need it for the GitHub Pages setup!

## Part 2: Set up GitHub Pages

### Step 1: Update index.html with your app URL

1. Open `index.html`
2. Replace **both** instances of `YOUR-APP-NAME.shinyapps.io/Verktyg/` with your actual shinyapps.io URL
3. Look for these lines (appears twice):

```html
<a href="https://YOUR-APP-NAME.shinyapps.io/Verktyg/" class="cta-button" target="_blank">
```

Replace with:

```html
<a href="https://YOUR-USERNAME.shinyapps.io/Verktyg/" class="cta-button" target="_blank">
```

### Step 2: Commit and push changes

```bash
git add index.html DEPLOYMENT.md
git commit -m "Add GitHub Pages website"
git push origin master
```

### Step 3: Enable GitHub Pages

1. Go to your GitHub repository: https://github.com/claesbackman/Verktyg
2. Click "Settings" (top menu)
3. Click "Pages" in the left sidebar
4. Under "Source", select:
   - **Branch**: `master` (or `main`)
   - **Folder**: `/ (root)`
5. Click "Save"

### Step 4: Wait for deployment

GitHub will build your site (takes 1-2 minutes). Your site will be available at:

```
https://claesbackman.github.io/Verktyg/
```

You can check the deployment status under "Actions" tab in your repository.

## Part 3: Test everything

1. Visit your GitHub Pages site: `https://claesbackman.github.io/Verktyg/`
2. Click the "Öppna kalkylatorn" button
3. Verify it opens your Shiny app on shinyapps.io
4. Test the app functionality

## Troubleshooting

### Shinyapps.io deployment issues

**Error: Package 'X' not found**
- Install the missing package in R: `install.packages("X")`
- Redeploy

**App crashes on startup**
- Check the logs in shinyapps.io dashboard
- Common issue: missing dependencies in `global.R`

**Deployment timeout**
- Your app may be too large. Try excluding unnecessary files:
  ```r
  rsconnect::deployApp(appFiles = c("ui.R", "server.R", "global.R"))
  ```

### GitHub Pages issues

**404 Page Not Found**
- Make sure you pushed `index.html` to the repository
- Verify GitHub Pages is enabled in Settings → Pages
- Check that you selected the correct branch

**Site not updating**
- Clear your browser cache
- Wait 5-10 minutes for GitHub's CDN to update
- Check Actions tab for build errors

**Shiny app link not working**
- Verify you updated **both** app URLs in `index.html`
- Make sure your shinyapps.io app is running (not archived due to inactivity)

## Updating your app

### To update the Shiny app:

```r
# Make changes to app/ui.R or app/server.R
library(rsconnect)
setwd("app")  # Navigate to app folder
rsconnect::deployApp(appName = "Verktyg")
```

### To update the GitHub Pages site:

```bash
# Edit index.html
git add index.html
git commit -m "Update website"
git push origin master
```

## Free tier limitations

**shinyapps.io free tier:**
- 25 active hours per month
- App sleeps after 15 minutes of inactivity
- Takes a few seconds to wake up when accessed

**Tips to manage usage:**
- Archive unused apps
- Monitor usage in your shinyapps.io dashboard
- Consider upgrading if you need more hours

## Custom domain (optional)

If you want a custom domain for GitHub Pages:

1. Buy a domain (e.g., from Namecheap, Google Domains)
2. In your repo settings → Pages → Custom domain
3. Add your domain and follow DNS configuration instructions

## Need help?

- **Shinyapps.io docs**: https://docs.posit.co/shinyapps.io/
- **GitHub Pages docs**: https://docs.github.com/en/pages
- **Shiny tutorial**: https://shiny.posit.co/r/getstarted/

## Summary checklist

- [ ] Create shinyapps.io account
- [ ] Install and configure rsconnect
- [ ] Deploy Shiny app to shinyapps.io
- [ ] Update index.html with your app URL
- [ ] Push changes to GitHub
- [ ] Enable GitHub Pages in repository settings
- [ ] Test both sites
- [ ] Share your new website!

Your sites:
- **Documentation**: https://claesbackman.github.io/Verktyg/
- **Live app**: https://YOUR-USERNAME.shinyapps.io/Verktyg/
