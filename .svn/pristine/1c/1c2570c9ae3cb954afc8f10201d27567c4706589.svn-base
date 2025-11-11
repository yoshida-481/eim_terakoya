package eim.command.common.util;

import java.io.*;
import eim.net.*;
import eim.util.EIMConfig;



public class EncryptService {
	
	private String SSH 				= "";
	private String SSH_HOST	 		= "";
	private String SSH_USER	 		= "";
	private String CMD_VERIFY	 	= "";
	private String CMD_VERSION		= "";

	
	private EIMSession sess = null;
	
	public EncryptService() {
	}
	public EncryptService(EIMSession sess) {
		this.sess = sess;
	}
	
	
	
	/**
	 * @return the sess
	 */
	public EIMSession getSess() {
		return sess;
	}
	/**
	 * @param sess the sess to set
	 */
	public void setSess(EIMSession sess) {
		this.sess = sess;
	}
	@SuppressWarnings("unchecked")
	public String version() throws Exception {
		
		//Process
		Process process = null;
		BufferedReader br = null;
		
		//get Config Parameter
		getConfig();
		
		//Command
		String cmd = SSH + " " + SSH_USER + "@" + SSH_HOST + " " + CMD_VERSION;
		String result = "unknown";
		
		//Execute
		try {
			process = Runtime.getRuntime().exec(cmd);
			br = new BufferedReader(new InputStreamReader(process.getInputStream()));
			result = br.readLine();
		} catch (Exception e) {
			throw e;
		}
		
		return result;
		
	}
	
	@SuppressWarnings("unchecked")
	public int verify(File file) throws Exception {
		
		//Process
		Process process = null;
		BufferedReader br = null;
		
		//get Config Parameter
		getConfig();

		//Command
		String cmd = SSH + " " + SSH_USER + "@" + SSH_HOST + " " + CMD_VERIFY + " " + file.getPath();
		String message = null;
		int code = -1;
		
		//Execute
		try {
			process = Runtime.getRuntime().exec(cmd);
			br = new BufferedReader(new InputStreamReader(process.getInputStream()));
			//br = new BufferedReader(new InputStreamReader(process.getInputStream(), "EUC-JP"));
			while((message = br.readLine()) != null) {
			}
			code = process.waitFor();
		} catch (Exception e) {
			throw e;
		}
		
		//Check Result For SSH Verify Command
		if(message != null && !message.equals("")) {
			try {
				code = Integer.parseInt(message);
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
		
		return code;
		
	}
	
	private void getConfig() throws Exception {
		SSH = EIMConfig.getValue("EIM.COMMAND.SSH");
		SSH_HOST = EIMConfig.getValue("EIM.COMMAND.SSH.HOST");
		SSH_USER = EIMConfig.getValue("EIM.COMMAND.SSH.USER");
		CMD_VERIFY = EIMConfig.getValue("EIM.COMMAND.VERIFY");
		CMD_VERSION = EIMConfig.getValue("EIM.COMMAND.VERSION");
		
	}
}
