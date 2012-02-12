using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using Wnms04.UpnpStack;
using Wnms04.Office;

namespace Project5UI
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Button button1;
		private System.Windows.Forms.Button button2;
		private System.Windows.Forms.Button button3;
		private System.Windows.Forms.ListBox listBox1;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

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
			if( disposing )
			{
				if (components != null) 
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.button1 = new System.Windows.Forms.Button();
			this.button2 = new System.Windows.Forms.Button();
			this.button3 = new System.Windows.Forms.Button();
			this.listBox1 = new System.Windows.Forms.ListBox();
			this.SuspendLayout();
			// 
			// button1
			// 
			this.button1.Location = new System.Drawing.Point(128, 8);
			this.button1.Name = "button1";
			this.button1.Size = new System.Drawing.Size(120, 24);
			this.button1.TabIndex = 0;
			this.button1.Text = "Stop Server";
			this.button1.Click += new System.EventHandler(this.button1_Click);
			// 
			// button2
			// 
			this.button2.Location = new System.Drawing.Point(8, 8);
			this.button2.Name = "button2";
			this.button2.Size = new System.Drawing.Size(112, 24);
			this.button2.TabIndex = 1;
			this.button2.Text = "Start Server";
			this.button2.Click += new System.EventHandler(this.button2_Click);
			// 
			// button3
			// 
			this.button3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
			this.button3.Location = new System.Drawing.Point(304, 8);
			this.button3.Name = "button3";
			this.button3.Size = new System.Drawing.Size(112, 24);
			this.button3.TabIndex = 2;
			this.button3.Text = "Save Log to File";
			this.button3.Click += new System.EventHandler(this.button3_Click);
			// 
			// listBox1
			// 
			this.listBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
				| System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right)));
			this.listBox1.Location = new System.Drawing.Point(8, 40);
			this.listBox1.Name = "listBox1";
			this.listBox1.Size = new System.Drawing.Size(408, 225);
			this.listBox1.TabIndex = 3;
			// 
			// Form1
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(424, 270);
			this.Controls.Add(this.listBox1);
			this.Controls.Add(this.button3);
			this.Controls.Add(this.button2);
			this.Controls.Add(this.button1);
			this.Name = "Form1";
			this.Text = "WNMS Project 5 - Group 4";
			this.Load += new System.EventHandler(this.Form1_Load);
			this.ResumeLayout(false);

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new Form1());
		}

		SampleDevice device=null;

		private void Log(string logText)
		{
			this.listBox1.Items.Add(String.Format("{0} ( {1} ) - {2}",System.DateTime.Now.ToShortDateString(),System.DateTime.Now.ToShortTimeString(),logText));
		}

		// Start Device
		private void button2_Click(object sender, System.EventArgs e)
		{
			if (device==null)
			{
				// Starting UPnP Device
				Log("Intel's UPnP .NET Framework Stack");
				Log("Intel Device Builder Build#1.0.1725.27554");
				device = new SampleDevice();
				device.OnLogEvent += new Wnms04.UpnpStack.SampleDevice.OnLog(this.Log);
				device.Start();
			}
			else
			{
				Log("Device Already Running.");
			}
		}

		private void button1_Click(object sender, System.EventArgs e)
		{
			if (device!=null)
			{
				// Starting UPnP Device
				Log("Stopping UPNP Device");
				device.Stop();
				device=null;  // Indicate we're done with this device...
				GC.Collect(); // and garbage collect to clean up the PPT controller
			}
			else
			{
				Log("Device is not running.");
			}
		}

		private void button3_Click(object sender, System.EventArgs e)
		{
			System.Windows.Forms.SaveFileDialog sfd = new SaveFileDialog();
			if (sfd.ShowDialog()==DialogResult.OK)
			{

				System.IO.StreamWriter sw = new System.IO.StreamWriter(sfd.FileName);
				foreach (object o in listBox1.Items)
				{
					sw.WriteLine(o.ToString());
				}
				sw.Close();
				Log("Log written.");
			}
		}

		private void Form1_Load(object sender, System.EventArgs e)
		{
		
		}
	}
}
