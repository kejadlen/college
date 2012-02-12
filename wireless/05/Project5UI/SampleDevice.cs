// Intel's UPnP .NET Framework Device Stack, Device Module
// Intel Device Builder Build#1.0.1725.27554

using System;
using Intel.UPNP;
using Wnms04.UpnpStack;

namespace Wnms04.UpnpStack
{
	/// <summary>
	/// Summary description for SampleDevice.
	/// </summary>
	class SampleDevice
	{
		private UPnPDevice device;
		
		public SampleDevice()
		{
			device = UPnPDevice.CreateRootDevice(1800,1.0,"\\");
			device.FriendlyName = "ProjectorDevice";
			device.Manufacturer = "WNMS Group 4";
			device.ManufacturerURL = "http://www.vt.edu";
			device.ModelName = "Projector Controller";
			device.ModelDescription = "Powerpoint Projector Controller";
			device.ModelNumber = "G4";
			device.HasPresentation = false;
			device.DeviceURN = "urn:schemas-upnp-org:device:projector:1";
			Wnms04.UpnpStack.P5_04 P5_04 = new Wnms04.UpnpStack.P5_04();
			
			P5_04.OnLogEvent += new P5_04.OnLog(this.BubbleLog);
			device.AddService(P5_04);
			
			
			// Setting the initial value of evented variables
			P5_04.Evented_PageMax = 0;
			P5_04.Evented_Power = false;
			P5_04.Evented_File = "Sample String";
			P5_04.Evented_PageNumber = 0;
			P5_04.Evented_Files = "Sample String";
		}
		
		public delegate void OnLog(string logData);
		public event OnLog OnLogEvent;

		public void BubbleLog(string logData)
		{
			OnLogEvent(logData);
		}

		public void Start()
		{
			device.StartDevice();
		}
		
		public void Stop()
		{
			device.StopDevice();
		}
		
		public void P5_04_GetFiles(out System.String Files)
		{
			Files = "Sample String";
			Console.WriteLine("P5_04_GetFiles(" + ")");
		}
		
		public void P5_04_GetStatus(out System.String File, out System.Boolean Power, out System.Int32 PageNumber)
		{
			File = "Sample String";
			Power = false;
			PageNumber = 0;
			Console.WriteLine("P5_04_GetStatus(" + ")");
		}
		
		public void P5_04_Go(System.Int32 PageNumber)
		{
			Console.WriteLine("P5_04_Go(" + PageNumber.ToString() + ")");
		}
		
		public void P5_04_NextPage(out System.Int32 PageNumber)
		{
			PageNumber = 0;
			Console.WriteLine("P5_04_NextPage(" + ")");
		}
		
		public void P5_04_PowerOff(out System.Boolean Power)
		{
			Power = false;
			Console.WriteLine("P5_04_PowerOff(" + ")");
		}
		
		public void P5_04_PowerOn(System.String File, out System.Int32 PageNumber, out System.Int32 PageMax, out System.Boolean Power)
		{
			PageNumber = 0;
			PageMax = 0;
			Power = false;
			Console.WriteLine("P5_04_PowerOn(" + File.ToString() + ")");
		}
		
		public void P5_04_PreviousPage(out System.Int32 PageNumber)
		{
			PageNumber = 0;
			Console.WriteLine("P5_04_PreviousPage(" + ")");
		}
		
	}
}

