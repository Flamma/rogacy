# How to push to GitHub

To push your roguelike game to your GitHub repository, follow these steps:

1. First, create a new repository on GitHub:
   - Go to https://github.com/new
   - Name it "rogacy" 
   - Don't initialize with README, .gitignore, or license

2. To push to GitHub, you'll need to:
   a. Create a personal access token:
      - Go to GitHub Settings > Developer settings > Personal access tokens
      - Generate new token with "repo" scope
      - Copy the token

   b. Set up your Git credentials:
      ```bash
      git config --global credential.helper store
      ```

   c. Push to GitHub:
      ```bash
      git push -u origin master
      ```

   When prompted for username, enter "Flamma"
   When prompted for password, enter your personal access token

3. Alternatively, you can use SSH:
   - Set up SSH keys: https://docs.github.com/en/authentication/connecting-to-github-with-ssh
   - Change the remote URL to: git@github.com:Flamma/rogacy.git