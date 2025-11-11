package eim.command.business.service.execute;

import java.util.ArrayList;


import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultDocument;
import eim.command.common.util.EIMCommandConstant;
import eim.command.common.util.EIMCommandUtil;
import eim.util.EIMConstant;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;

/**
 * getAttrコマンド用コマンド実行実装クラス
 */
public class EIMCommandGetAttrExecuter extends EIMCommandExecuter {
	//引数objIdを格納する配列
	private ArrayList<String> objIds = new ArrayList<String>();
	public EIMCommandGetAttrExecuter() {
	}

    public EIMCommandResult execute() throws Exception {

		EIMObject object = null;
		EIMCommandResultDocument result = null;
		try {

			result = new EIMCommandResultDocument(getSess());

			// objIdが正常に入力されているかパラメータのチェック
			if (!isParamCheckAndEntryObjId(result)) {
				return result;
			}

			// EIMObjectの取得
			object = ObjectUtils.getObjectById(getSess(), Long.valueOf(objIds
					.get(0)));

			// 指定したオブジェクトが存在しない
			if (object == null) {
				setErrorCodeMessage(result, "EIM.ERROR.CODE.OJBECT.NO.EXIST",
						"EIM.ERROR.LOGIC.OJBECT.NO.EXIST");
				return result;
			}

			//読み取り権限チェック
			if(!SecurityUtils.authorized(getSess(), object, getSess().getUser(), EIMAccessRole.READ))
			{
				setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.READ.AUTH", "EIM.ERROR.LOGIC.OJBECT.NO.EXIST");
				return result;
			}

			// オブジェクト情報を実行結果に設定
			if (result.getType() == null)
				result.setType(EIMResource
						.getMessageValue("EIM.RESULT.TYPE.INFO"));
			object = ObjectUtils.getObjectById(getSess(), object.getId());
			result.setTarget(object);

			// 操作履歴
			this.createOperationHistory(
					EIMCommandConstant.GET_ATTRIBUTE_EXCOMMAND,
					EIMCommandConstant.TARGET_TO_GET_OBJECT_INFO, object);
			// アクセス履歴
			this.createAccessHistory(object, "EIM.ACCESS.TYPE.EXIF.GETATTRIBUTEINFO");
			return result;

		} catch (Exception e) {
			setErrorCodeMessage(result, "EIM.ERROR.CODE.SYSTEM.ERROR",
					"EIM.ERROR.LOGIC.SYSTEM.ERROR");
			throw e;
		}

	}

    /**
	 * @return boolean
     */
	public ArrayList<String> getObjIdArray() {
		return objIds;
	}

    /**
     * objIdsリストにobjIdを格納する
	 * @param String
     */
	public void setObjId(String objId) {
		objIds.add(objId);
	}

	/* (非 Javadoc)
	 * @see command.business.service.execute.EIMCommandExecuter#setOtherParameter(java.lang.String, java.lang.String)
	 */
	@Override
	public void setOtherParameter(String key, String value) {
		if(key.equals(EIMCommandConstant.OBJID)) {
			this.setObjId(value);
		}
		super.setOtherParameter(key, value);
	}

	@Override
	protected void createOperationHistory(String operationTypeNo, String targetInfoNo, EIMObject targetObj) throws Exception {

		OperationHistoryUtils.create(getSess(), EIMCommandConstant.COMMAND, operationTypeNo, targetInfoNo,
										targetObj!=null ? EIMConstant.OBJECT : null, targetObj, null, null, null, null);
	}

    /**
     * パラメータ解析してからObjIdを格納する
	 * @param EIMCommandResultDocument
	 * @return boolean
	 * @throws EIMException
     */
    private boolean isParamCheckAndEntryObjId(EIMCommandResultDocument result) throws EIMException{
    	//objIdがあるか
		if (objIds.size() < 1) {
			setErrorCodeMessage(result, "EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER", "EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", EIMCommandConstant.OBJID);
			return false;
		}
		// objIdが複数指定されていないか
		if (objIds.size() > 1) {
			setErrorCodeMessage(result, "EIM.ERROR.CODE.OVERLAPPING.SINGLE.PARAMETER", "EIM.ERROR.LOGIC.OVERLAPPING.SINGLE.PARAMETER", EIMCommandConstant.OBJID);
			return false;
		}
		// objIdが半角数字であるか
		if (!EIMCommandUtil.isOneByteNum(objIds.get(0))){
			setErrorCodeMessage(result, "EIM.ERROR.CODE.OBJID.NOT.NUM", "EIM.ERROR.LOGIC.OBJID.NOT.NUM", objIds.get(0));
			return false;
		}
		return true;
    }

	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @throws EIMException
	 */
	private void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), EIMResource.getMessageValue(messageKey));
	}

	/**
	 * エラー情報を設定
	 * @param resultData
	 * @param codeKey
	 * @param messageKey
	 * @param replacement メッセージ中の{0}を指定の文字列に置換する
	 * @throws EIMException
	 */
	private void setErrorCodeMessage(EIMCommandResult resultData, String codeKey, String messageKey, String replacement) throws EIMException{
		resultData.setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue(codeKey), (EIMResource.getMessageValue(messageKey)).replace("{0}", replacement));
	}

}