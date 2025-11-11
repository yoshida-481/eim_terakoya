package eim.command.business.service.factroy;

import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultData;
import eim.command.common.util.EIMCommandConstant;

public class EIMCommandResultFactory {
	
	/**
	 * コマンドごとの実行結果格納クラスインスタンスを生成する。
	 * @param cmdStr
	 * @return
	 * @throws Exception
	 */
	public EIMCommandResult create(String cmdStr) throws Exception
	{
		EIMCommandResult result = null;
		
		if (cmdStr.equals(EIMCommandConstant.GET)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_BY_ID)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.PUT)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.LS)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.UPDATE_ATTR)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.FIND)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_ATTR)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_IN_BY_USER)){
			result = new EIMCommandResultData();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_OUT_BY_USER)){
			result = new EIMCommandResultData();
		}
		else if (cmdStr.equals(EIMCommandConstant.ENC)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.VERIFY)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.LOGIN)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.SELECT)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.DELETE)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.CREATE)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.MKDIR)){
			result = new EIMCommandResult();
		}
		else if (cmdStr.equals(EIMCommandConstant.RM)){
			result = new EIMCommandResult();
		}
		else {
			throw new Exception();
		}
		
		return result;
	}

}
