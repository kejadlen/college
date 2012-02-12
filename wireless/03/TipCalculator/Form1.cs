using System;
using System.Drawing;
using System.Collections;
using System.Windows.Forms;
using System.Data;

namespace TipCalculator
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.TextBox txtBill;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.RadioButton rb15;
		private System.Windows.Forms.RadioButton rb175;
		private System.Windows.Forms.RadioButton rb20;
		private System.Windows.Forms.Button btnCalc;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.Label label5;
		private System.Windows.Forms.Label lblTipAmount;
		private System.Windows.Forms.Label lblTotal;
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
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.txtBill = new System.Windows.Forms.TextBox();
			this.label3 = new System.Windows.Forms.Label();
			this.rb15 = new System.Windows.Forms.RadioButton();
			this.rb175 = new System.Windows.Forms.RadioButton();
			this.rb20 = new System.Windows.Forms.RadioButton();
			this.btnCalc = new System.Windows.Forms.Button();
			this.label4 = new System.Windows.Forms.Label();
			this.label5 = new System.Windows.Forms.Label();
			this.lblTipAmount = new System.Windows.Forms.Label();
			this.lblTotal = new System.Windows.Forms.Label();
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(24, 8);
			this.label1.Size = new System.Drawing.Size(192, 16);
			this.label1.Text = "Ben and Alpha\'s Tip Calculator";
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(8, 32);
			this.label2.Size = new System.Drawing.Size(64, 16);
			this.label2.Text = "Bill";
			// 
			// txtBill
			// 
			this.txtBill.Location = new System.Drawing.Point(96, 32);
			this.txtBill.Size = new System.Drawing.Size(128, 22);
			this.txtBill.Text = "";
			// 
			// label3
			// 
			this.label3.Location = new System.Drawing.Point(8, 72);
			this.label3.Size = new System.Drawing.Size(72, 16);
			this.label3.Text = "Tip";
			// 
			// rb15
			// 
			this.rb15.Checked = true;
			this.rb15.Location = new System.Drawing.Point(96, 72);
			this.rb15.Text = "15%";
			// 
			// rb175
			// 
			this.rb175.Location = new System.Drawing.Point(96, 96);
			this.rb175.Text = "17.5%";
			// 
			// rb20
			// 
			this.rb20.Location = new System.Drawing.Point(96, 120);
			this.rb20.Text = "20%";
			// 
			// btnCalc
			// 
			this.btnCalc.Location = new System.Drawing.Point(96, 144);
			this.btnCalc.Size = new System.Drawing.Size(128, 20);
			this.btnCalc.Text = "Calculate";
			this.btnCalc.Click += new System.EventHandler(this.btnCalc_Click);
			// 
			// label4
			// 
			this.label4.Location = new System.Drawing.Point(8, 184);
			this.label4.Size = new System.Drawing.Size(80, 16);
			this.label4.Text = "Tip Amount";
			// 
			// label5
			// 
			this.label5.Location = new System.Drawing.Point(8, 208);
			this.label5.Size = new System.Drawing.Size(64, 20);
			this.label5.Text = "Total";
			// 
			// lblTipAmount
			// 
			this.lblTipAmount.Location = new System.Drawing.Point(96, 184);
			// 
			// lblTotal
			// 
			this.lblTotal.Location = new System.Drawing.Point(96, 208);
			// 
			// Form1
			// 
			this.Controls.Add(this.lblTotal);
			this.Controls.Add(this.lblTipAmount);
			this.Controls.Add(this.label5);
			this.Controls.Add(this.label4);
			this.Controls.Add(this.btnCalc);
			this.Controls.Add(this.rb20);
			this.Controls.Add(this.rb175);
			this.Controls.Add(this.rb15);
			this.Controls.Add(this.label3);
			this.Controls.Add(this.txtBill);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Menu = this.mainMenu1;
			this.Text = "Form1";

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>

		static void Main() 
		{
			Application.Run(new Form1());
		}

    // Calculate the tip and display it and the total bill.
		private void btnCalc_Click(object sender, System.EventArgs e)
		{
			decimal bill = System.Decimal.Parse(this.txtBill.Text);
			this.txtBill.Text = String.Format("{0:c}",bill);
			decimal tipPercentage = 0;

			if (this.rb15.Checked)
			{
				tipPercentage = 0.15M;
			}
			else if (this.rb175.Checked)
			{
				tipPercentage = 0.175M;
			}
			else if (this.rb20.Checked)
			{
				tipPercentage = 0.20M;
			}
			else
			{
				throw new ArgumentOutOfRangeException("Select a tip percentage");
			}

			decimal tipAmount=bill*tipPercentage;
			decimal total=tipAmount+bill;
			
			this.lblTipAmount.Text=String.Format("{0:c}",tipAmount);
			this.lblTotal.Text=String.Format("{0:c}",total);
		}
	}
}
