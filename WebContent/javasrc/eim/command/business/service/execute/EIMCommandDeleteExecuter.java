package eim.command.business.service.execute;

import eim.bo.EIMObject;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandResultConstant;
import eim.net.EIMSession;
import eim.util.ObjectUtils;

public class EIMCommandDeleteExecuter extends EIMCommandExecuter {

	@Override
	public EIMCommandResult execute() throws Exception {
		//Session
		EIMSession sess = super.getSess();
		if(sess == null) {
			throw new Exception(EIMCommandResultConstant.MSG_AUTH_FAILED);
		}
		
		//Get ID
		int objId = 0;
		if(super.getOtherParameter("id") == null || super.getOtherParameter("id").equals("")) {
			throw new Exception("オブジェクトIDが取得できません" + "(" + super.getOtherParameter("id") + ")");
		}
		try {
			objId = Integer.parseInt(super.getOtherParameter("id"));
		} catch(Exception e) {
			throw new Exception("IDの形式が不正です" + "(" + super.getOtherParameter("id") + ")");
		}
		
		//Get Object by ID
		EIMObject object = ObjectUtils.getObjectById(sess, objId);
		if(object == null) {
			throw new Exception("オブジェクトが取得できません" + "(" + objId + ")");
		}
		EIMCommandResult result = new EIMCommandResult();
		
		try {
			
			//Delete Object
			ObjectUtils.deleteObject(sess, object);
			
			//Commit
			sess.commit();
			
			//Message
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, "オブジェクトを削除しました" + "(" + objId + ")");
		
		} catch(Exception e) {
			
			//Rollback
			sess.rollback();
			
			//Result
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_ERROR, EIMCommandResultConstant.CODE_SYSTEM_ERROR, e.getMessage() + "(" + objId + ")");
			
		} finally {
			sess.close();
		}
		
		return result;
		
	}
	
}
