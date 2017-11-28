<h1>Overview</h1>

<p>The ubeRpay package is primarily an accounting tool that will help you produce income statements for your Uber or UberEats partner business. Its primary functions are to:</p>

<ul>
<li>Read PDF pay statements and convert the text to a dataframe</li>
<li>Calculate quarterly and annual income and expense summaries</li>
<li>Produce code for a Latex table showing the income statement up to EBIT. </li>
</ul>

<h1>Installation</h1>

<p><code>{r, eval=FALSE}
devtools::install_github("SmarshMELLOW/ubeRpay")
</code></p>

<p>I have only tested this function with delivery data (<em>i.e.</em> UberEats), so if you have an example pay stub from driving (<em>i.e.</em> Uber) that you wouldn't mind sharing, please e-mail it to me. Also, if you encounter a bug, please e-mail me your code and I will work on a patch.</p>

<h1>Usage</h1>

<p>Currently, ubeRpay does not scrape data from the payments site. To download the PDFs of your pay stubs, go to Uber's <a href="" title="https://partners.uber.com/p3/money/statements/all">partner payments page</a> and click on view statement. Scroll to the bottom of the page and click on Print Statement. This should open a new window with a PDF showing your payments. Save the file as a PDF using landscape mode, A4 paper and minimal or no margins. </p>

<p>Once you have saved all of your pay stubs in a folder, you can batch import them.</p>

<p>```
    stub.dir &lt;- "/Users/SMARSH/UBER Income/pay statements/"</p>

<pre><code>pay.files &lt;- paste0(stub.dir, list.files(stub.dir))

deliv.data &lt;- batch_table_trips( pay.files )
</code></pre>

<p>```</p>

<h1>New To Uber</h1>

<p>Sign up to drive or deliver with Uber/UberEats using code <a href="" title="https://partners.uber.com/i/samm387">samm387</a> and you should receive some bonus after completing 25 trips.</p>

