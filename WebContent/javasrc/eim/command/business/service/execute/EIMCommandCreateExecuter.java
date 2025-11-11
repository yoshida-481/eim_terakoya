package eim.command.business.service.execute;

import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandResultConstant;
import eim.net.EIMSession;
import eim.util.ObjectUtils;

public class EIMCommandCreateExecuter extends EIMCommandExecuter {

	@Override
	public EIMCommandResult execute() throws Exception {
		//Session
		EIMSession sess = super.getSess();
		if(sess == null) {
			throw new Exception(EIMCommandResultConstant.MSG_AUTH_FAILED);
		}
		
		//Get Object Type
		if(super.getOtherParameter("type") == null || super.getOtherParameter("type").equals("")) {
			throw new Exception("オブジェクトタイプが取得できません" + "(" + super.getOtherParameter("type") + ")");
		}
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, super.getOtherParameter("type"));
		if(objType == null) {
			throw new Exception("オブジェクトタイプが取得できません" + "(" + super.getOtherParameter("type") + ")");
		}
		
		//Get Object Name
		if(super.getOtherParameter("name") == null || super.getOtherParameter("name").equals("")) {
			throw new Exception("オブジェクトの名称が取得できません" + "(" + super.getOtherParameter("name") + ")");
		}
		
		//Create Object
		EIMObject object = ObjectUtils.createObject(sess, objType, super.getOtherParameter("name"));
		
		//Commit
		sess.commit();
		
		
		return setData(object);
		
	}
	
	@SuppressWarnings("unchecked")
	public EIMCommandResult setData(EIMObject object) throws Exception {
		EIMCommandResult result = new EIMCommandResult();
		result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.NONE);
			result.setData(new EIMObject[]{object});
		return result;
	}

}
