using System;
using System.Drawing;
using System.Collections;
using System.Windows.Forms;
using System.Data;

namespace PizzaShack
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.CheckBox cbxPepperoni;
		private System.Windows.Forms.CheckBox cbxSausage;
		private System.Windows.Forms.CheckBox cbxHam;
		private System.Windows.Forms.CheckBox cbxPeppers;
		private System.Windows.Forms.CheckBox cbxOnions;
		private System.Windows.Forms.CheckBox cbxPineapple;
		private System.Windows.Forms.ComboBox cbxSize;
		private System.Windows.Forms.ComboBox cbxCrust;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.Button btnCalculatePrice;
		private System.Windows.Forms.Label lblPrice;
		private System.Windows.Forms.MainMenu mainMenu1;

		
		public Form1()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}
		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			base.Dispose( disposing );
		}
		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.label3 = new System.Windows.Forms.Label();
			this.cbxPepperoni = new System.Windows.Forms.CheckBox();
			this.cbxSausage = new System.Windows.Forms.CheckBox();
			this.cbxHam = new System.Windows.Forms.CheckBox();
			this.cbxPeppers = new System.Windows.Forms.CheckBox();
			this.cbxOnions = new System.Windows.Forms.CheckBox();
			this.cbxPineapple = new System.Windows.Forms.CheckBox();
			this.cbxSize = new System.Windows.Forms.ComboBox();
			this.cbxCrust = new System.Windows.Forms.ComboBox();
			this.label4 = new System.Windows.Forms.Label();
			this.btnCalculatePrice = new System.Windows.Forms.Button();
			this.lblPrice = new System.Windows.Forms.Label();
			// 
			// label1
			// 
			this.label1.Font = new System.Drawing.Font("Tahoma", 9F, System.Drawing.FontStyle.Bold);
			this.label1.Location = new System.Drawing.Point(8, 8);
			this.label1.Size = new System.Drawing.Size(224, 16);
			this.label1.Text = "Alpha Pizzeria";
			this.label1.TextAlign = System.Drawing.ContentAlignment.TopCenter;
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(8, 32);
			this.label2.Size = new System.Drawing.Size(88, 16);
			this.label2.Text = "Size";
			// 
			// label3
			// 
			this.label3.Location = new System.Drawing.Point(8, 64);
			this.label3.Size = new System.Drawing.Size(88, 20);
			this.label3.Text = "Crust Type";
			// 
			// cbxPepperoni
			// 
			this.cbxPepperoni.Location = new System.Drawing.Point(16, 120);
			this.cbxPepperoni.Text = "Pepperoni";
			// 
			// cbxSausage
			// 
			this.cbxSausage.Location = new System.Drawing.Point(16, 144);
			this.cbxSausage.Text = "Sausage";
			// 
			// cbxHam
			// 
			this.cbxHam.Location = new System.Drawing.Point(16, 168);
			this.cbxHam.Text = "Ham";
			// 
			// cbxPeppers
			// 
			this.cbxPeppers.Location = new System.Drawing.Point(120, 120);
			this.cbxPeppers.Text = "Green Peppers";
			// 
			// cbxOnions
			// 
			this.cbxOnions.Location = new System.Drawing.Point(120, 144);
			this.cbxOnions.Text = "Onions";
			// 
			// cbxPineapple
			// 
			this.cbxPineapple.Location = new System.Drawing.Point(120, 168);
			this.cbxPineapple.Text = "Pineapple";
			
			// 
			// cbxSize - use a Dictionary for the sizes and costs
			// 
			System.Collections.ArrayList sizes = new ArrayList();
			sizes.Add(new DictionaryEntry("$6 - Small",6.00M));
			sizes.Add(new DictionaryEntry("$9 - Medium",9.00M));
			sizes.Add(new DictionaryEntry("$11 - Large",11.00M));
			this.cbxSize.DataSource = sizes;
			this.cbxSize.DisplayMember="Key";
			this.cbxSize.ValueMember="Value";
			
			this.cbxSize.Location = new System.Drawing.Point(112, 32);
			this.cbxSize.Size = new System.Drawing.Size(104, 22);
			
			// 
			// cbxCrust - use a Dictionary for the crusts and costs
			// 
			System.Collections.ArrayList crusts=new ArrayList();
			crusts.Add(new DictionaryEntry("$0 - Thin Crust",0.00M));
			crusts.Add(new DictionaryEntry("$0 - Original",0.00M));
			crusts.Add(new DictionaryEntry("$1 - Sicilian",1.00M));
			this.cbxCrust.DataSource=crusts;
			this.cbxCrust.DisplayMember="Key";
			this.cbxCrust.ValueMember="Value";	
			this.cbxCrust.Location = new System.Drawing.Point(112, 64);
			this.cbxCrust.Size = new System.Drawing.Size(104, 22);
			
			// 
			// label4
			// 
			this.label4.Location = new System.Drawing.Point(8, 96);
			this.label4.Size = new System.Drawing.Size(112, 16);
			this.label4.Text = "Toppings:";
			// 
			// btnCalculatePrice
			// 
			this.btnCalculatePrice.Location = new System.Drawing.Point(24, 224);
			this.btnCalculatePrice.Text = "Price:";
			this.btnCalculatePrice.Click += new System.EventHandler(this.btnCalculatePrice_Click);
			// 
			// lblPrice
			// 
			this.lblPrice.Location = new System.Drawing.Point(112, 224);
			// 
			// Form1
			// 
			this.Controls.Add(this.lblPrice);
			this.Controls.Add(this.btnCalculatePrice);
			this.Controls.Add(this.label4);
			this.Controls.Add(this.cbxCrust);
			this.Controls.Add(this.cbxSize);
			this.Controls.Add(this.cbxPineapple);
			this.Controls.Add(this.cbxOnions);
			this.Controls.Add(this.cbxPeppers);
			this.Controls.Add(this.cbxHam);
			this.Controls.Add(this.cbxSausage);
			this.Controls.Add(this.cbxPepperoni);
			this.Controls.Add(this.label3);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Text = "Pizzeria";

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>

		static void Main() 
		{
			Application.Run(new Form1());
		}

    // Find the cost of a single topping (based on the size)
		private decimal getToppingCostFromSizeCost(object sizeCost)
		{
			switch(Int32.Parse(sizeCost.ToString()))
			{
				case 6: return 0.71M;
				case 9: return 0.81M;
				case 11: return 0.91M;
				default: throw new ArgumentOutOfRangeException();
			}
		}
		private void btnCalculatePrice_Click(object sender, System.EventArgs e)
		{
		  // Get the initial cost (from the size)
			decimal pizzaPrice=(decimal)this.cbxSize.SelectedValue;

      // Add the cost of the crust
			pizzaPrice+=(decimal)this.cbxCrust.SelectedValue;

      // Add the cost of each topping
			foreach(System.Windows.Forms.Control c in this.Controls)
			{
				if (c.GetType() == typeof(System.Windows.Forms.CheckBox))
				{
					pizzaPrice += ((System.Windows.Forms.CheckBox)c).Checked ? getToppingCostFromSizeCost(this.cbxSize.SelectedValue) : 0M;
				}
			}

      // Output the final cost
			this.lblPrice.Text = String.Format("{0:c}",pizzaPrice);
		}
	}
}
