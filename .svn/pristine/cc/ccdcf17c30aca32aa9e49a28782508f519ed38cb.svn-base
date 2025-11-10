package eim.command.business.service.execute;

import java.util.ArrayList;
import java.util.List;

import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandResultConstant;
import eim.net.EIMSession;
import eim.util.ObjectUtils;

public class EIMCommandSelectExecuter extends EIMCommandExecuter {
	
	@SuppressWarnings("unchecked")
	@Override
	public EIMCommandResult execute() throws Exception {
		//Session
		EIMSession sess = super.getSess();
		if(sess == null) {
			throw new Exception(EIMCommandResultConstant.MSG_AUTH_FAILED);
		}
		
		//Result
		List<EIMObject> result = new ArrayList<EIMObject>();
		
		//Get Parameters
		if(super.getOtherParameter("id") != null) {
			
			//Get ID
			int objId = 0;
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
			result.add(object);
			
		} else {
			
			//Get Object Type
			if(super.getOtherParameter("type") == null || super.getOtherParameter("type").equals("")) {
				throw new Exception("オブジェクトタイプが取得できません" + "(" + super.getOtherParameter("type") + ")");
			}
			EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, super.getOtherParameter("type"));
			if(objType == null) {
				throw new Exception("オブジェクトタイプが取得できません" + "(" + super.getOtherParameter("type") + ")");
			}
			
			//Check Name Condition
			if(super.getOtherParameter("name") == null || super.getOtherParameter("name").equals("")) {
				result = ObjectUtils.getObjectListByType(sess, objType);
			} else {
				result = ObjectUtils.getObjectListByTypeAndName(sess, objType, super.getOtherParameter("name"));
			}
			
		}
		
			
		return setMessage(result);
		
	}
	
	@SuppressWarnings("unchecked")
	public EIMCommandResult setMessage(List<EIMObject> objList) throws Exception {
		EIMCommandResult result = new EIMCommandResult();
		
		//Check Object Size is Zero
		if(objList == null || objList.size() == 0) {
			result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.NONE);
			return result;
		}
		
		//Named Sort
		//objList = AppObjectUtil.getStrSortedList(objList, "getName", false);
		
		//Set Result Data
		result.setTypeCodeMessage(EIMCommandResultConstant.TYPE_INFO, EIMCommandResultConstant.NONE, EIMCommandResultConstant.NONE);
			result.setData((EIMObject[])objList.toArray(new EIMObject[0]));

		return result;
		
	}
	
}
