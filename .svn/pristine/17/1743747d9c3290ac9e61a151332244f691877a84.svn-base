package eim.command.business.service.execute;

import java.util.List;

import eim.bo.EIMObject;
import eim.bo.EIMUser;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandResultConstant;
import eim.net.EIMSession;
import eim.util.CipherUtils;
import eim.util.UserUtils;

public class EIMCommandLoginExecuter extends EIMCommandExecuter {

	@Override
	public EIMCommandResult execute() throws Exception {
		
		//Session
		EIMSession sess = super.getSess();
		
		//Get User
		EIMUser user = UserUtils.getUserByCode(sess, super.getUser());
		if(user == null) {
			throw new Exception("Invalid User [" + super.getUser() + "]");
		}
		
		//Check Password
		if(!user.getPass().equals(CipherUtils.getMessageDigest(sess, super.getPass()))) {
			throw new Exception("Invalid Password for User [" + super.getUser() + "]");
		}
		
		//Result
		EIMCommandResult result = new EIMCommandResult();
			result.setType(EIMCommandResultConstant.TYPE_INFO);
			result.setCode(EIMCommandResultConstant.NONE);
			result.setMessage("Welcome to EIMANAGER Command User Interface.");
		
		return result;
		
	}
	
	@SuppressWarnings("unchecked")
	public void setMessage(List<EIMObject> objList) throws Exception {
		String message = "";
		if(objList == null || objList.size() == 0) {
			message = "Child is Zero";
		}
		for(EIMObject object : objList) {
			message += object.getName() + "\n";
		}
		super.setMessage(message);
	}
	
}
