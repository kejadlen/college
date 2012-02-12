using System;
using System.Runtime.InteropServices;

namespace Wnms04.Office
{
	/// <summary>
	/// Summary description for UnsafeNativeMethods.
	/// </summary>
	
	public class PowerPointController : System.IDisposable
	{
		private bool m_isDisposed=false;
		private bool m_power=false;
		private int pageNumber=-1;
		private int maxPageNumber=-1;
		public PowerPointController()
		{

		}
		
		~PowerPointController()
		{
			Dispose();
		}


		private void VerifyNotDisposed()
		{
			if (m_isDisposed)
			{
				throw new InvalidOperationException("Operation can not be completed because object has been disposed.");
			}
		}

		public void OpenPresentation(string presentationFile)
		{
			VerifyNotDisposed();
			if (!System.IO.File.Exists(presentationFile))
			{
				throw new System.IO.FileNotFoundException("Presentation does not exist.",presentationFile);
			}
			PPT_InitInstance();
			maxPageNumber=PPT_Start(presentationFile);
			m_power=true;
			pageNumber=1;
		}

		public void ClosePresentation()
		{
			VerifyNotDisposed();
			pageNumber=-1;
			maxPageNumber=-1;
			PPT_Stop();
			PPT_ExitInstance();
			PPT_InitInstance();
			m_power=false;
		}

		public void NextSlide()
		{
			pageNumber++;
			if (pageNumber>maxPageNumber)
			{
				pageNumber=maxPageNumber;
			}
			PPT_Next();
		}

		public void PreviousSlide()
		{
			pageNumber--;
			if (pageNumber<0)
			{
				pageNumber=0;
			}
			PPT_Previous();
		}

		public void GotoSlide(long slideNumber)
		{
			int slide=(int)slideNumber;
			if (slide<1)
			{
				slide=1;
			}
			else if (slide>maxPageNumber)
			{
				slide=maxPageNumber;
			}
			pageNumber=slide;
			PPT_Goto(pageNumber);
		}

		public int PageNumber
		{
			get
			{
				return pageNumber;
			}
			set
			{
				GotoSlide((long)value);
			}
		}
		public int MaxPageNumber
		{
			get
			{
				return maxPageNumber;
			}
		}
		public bool Power
		{
			get
			{
				return m_power;
			}
		}

		/// <summary>
		/// Opens PowerPoint (if it is not already open), opens
		/// the file named in PPTFileName and starts the slide show.
		/// </summary>
		/// <param name="PPTFileName">The name of the file to open.</param>
		/// <returns>Returns the number of slides in the presentation.</returns>
		[DllImport("pptcontroller.dll", EntryPoint="PPT_Start")]
		private static extern int PPT_Start(string PPTFileName);

		[DllImport("pptcontroller.dll", EntryPoint="PPT_Next")]
		private static extern void PPT_Next();
		
		[DllImport("pptcontroller.dll", EntryPoint="PPT_Previous")]
		private static extern void PPT_Previous();

		[DllImport("pptcontroller.dll", EntryPoint="PPT_Stop")]
		private static extern void PPT_Stop();

		[DllImport("pptcontroller.dll", EntryPoint="PPT_Goto")]
		private static extern void PPT_Goto(long page);

		[DllImport("pptcontroller.dll", EntryPoint="PPT_InitInstance")]
		private static extern void PPT_InitInstance();

		[DllImport("pptcontroller.dll", EntryPoint="PPT_ExitInstance")]
		private static extern void PPT_ExitInstance();

		#region IDisposable Members

		public void Dispose()
		{
			try 
			{
				PPT_Stop();
				PPT_ExitInstance();
			}
			finally
			{
				m_isDisposed=true;
			}
		}

		#endregion
	}
}
