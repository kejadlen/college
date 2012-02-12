using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Web;
using System.Web.Services;

namespace PizzaService1
{
	/// <summary>
	/// Summary description for Service1.
	/// </summary>
	public class Service1 : System.Web.Services.WebService
	{
		public Service1()
		{
			//CODEGEN: This call is required by the ASP.NET Web Services Designer
			InitializeComponent();
		}

		#region Component Designer generated code
		
		//Required by the Web Services Designer 
		private IContainer components = null;
				
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if(disposing && components != null)
			{
				components.Dispose();
			}
			base.Dispose(disposing);		
		}
		
		#endregion

		// WEB SERVICE EXAMPLE
		// The HelloWorld() example service returns the string Hello World
		// To build, uncomment the following lines then save and build the project
		// To test this web service, press F5

		[WebMethod]
		public string HelloWorld()
		{
			return "Hello World";
		}

		[WebMethod]
		public string PlaceOrder(pizzaOrder order) 
		{
			System.Text.StringBuilder sb = new System.Text.StringBuilder(order.serverID.ToString());
			sb.Append(" ");
			if (order.crust==0)
			{
				sb.Append("Thin Crust ");
			}
			else if (order.crust==1)
			{
				sb.Append("Original ");
			}
			else if (order.crust==2)
			{
				sb.Append("Sicilian ");
			}

			if (order.size==0)
			{
				sb.Append("Small ");
			}
			else if (order.size==1)
			{
				sb.Append("Medium ");
			}
			else if (order.size==2)
			{
				sb.Append("Large ");
			}

			if (order.ham)
			{
				sb.Append("Ham ");
			}
			if (order.onions)
			{
				sb.Append("Onions ");
			}
			if (order.pepperoni)
			{
				sb.Append("Pepperoni ");
			}
			if (order.peppers)
			{
				sb.Append("Peppers ");
			}
			if (order.pineapple)
			{
				sb.Append("Pineapple ");
			}
			if (order.sausage)
			{
				sb.Append("Sausage ");
			}
			
			return sb.ToString().Trim();
		}
        
		public class pizzaOrder 
		{
        
			/// <remarks/>
			public int size;
        
			/// <remarks/>
			public int crust;
        
			/// <remarks/>
			public int serverID;
        
			/// <remarks/>
			public bool pepperoni;
        
			/// <remarks/>
			public bool sausage;
        
			/// <remarks/>
			public bool ham;
        
			/// <remarks/>
			public bool peppers;
        
			/// <remarks/>
			public bool onions;
        
			/// <remarks/>
			public bool pineapple;
		}
	}
}
