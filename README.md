# Impatient Markup

Convert & preview markup(md, rst) to html on the fly!



# Install

## Step 1: Install Pandoc

If you are using ubuntu, you can install it using

    sudo apt-get install pandoc

For other operating systms pelase visit [Pandoc installation guide](http://pandoc.org/installing.html)


## Step 2: Install impatient-markup


### From Github

Clone repo to your system

    git cone https://github.com/ChillarAnand/impatient-markup.git

Load impatient-markup.el and enable impatient markup

    (load-file "/path/to/impatient-markup.el")
    (impatient-markup-enable)

This enable impatient-markup in `markdown` & `rst-mode`.

You can enable it in other modes using hook.

    (add-hook 'impatient-markup 'markdown-mode)
































