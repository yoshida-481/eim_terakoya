package eimtest.app.util.net;

import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;

/** */
abstract public class SocketListener {
	/** */
	ListenerThread listenerThread;

	/** */
	boolean doPrintTrace = false;

	/** */
	public SocketListener() {
		listenerThread = getListerThread();
	}

	/**
	 * 
	 * @return o
	 */
	public SocketListener start()
	{
		listenerThread.start();
		return this;
	}

	/**
	 * 
	 * @param doTrace
	 * @return o
	 */
	public SocketListener setDoPrintTrace(boolean doTrace)
	{
		doPrintTrace = doTrace;
		return this;
	}

	/**
	 * 
	 * @return o
	 */
	public boolean isInService()
	{
		return listenerThread.isAlive();
	}

	/** */
	public void terminate()
	{
		listenerThread.terminate = true;
	}

	/**
	 * 
	 * @return o
	 */
	abstract protected ListenerThread getListerThread();

	/** */
	abstract protected class ListenerThread extends Thread {
		/** */
		int listenPort;

		/**
		 * 
		 * @param listenPort
		 */
		ListenerThread(int listenPort) {
			this.setDaemon(true);
			this.listenPort = listenPort;
		}

		/** */
		boolean terminate = false;

		/** */
		public void run()
		{
			try
			{
				ServerSocket listenSock = new ServerSocket(listenPort);
				listenSock.setSoTimeout(100);
				while (!terminate)
				{
					sleep(1000);
					Socket acceptSock;
					try
					{
						acceptSock = listenSock.accept();
					} catch (SocketTimeoutException e)
					{
						continue;
					}

					if (!acceptSock.getInetAddress().isLoopbackAddress())
					{
						System.err.println("deny access from " + acceptSock);
						acceptSock.close();
						continue;
					}
					try
					{
						process(acceptSock);
					} finally
					{
						acceptSock.close();
					}
				}
			} catch (Exception e)
			{
				e.printStackTrace();
			}
		}

		/**
		 * 
		 * @param acceptSock
		 * @throws Exception
		 */
		abstract void process(Socket acceptSock) throws Exception;
	}
}
