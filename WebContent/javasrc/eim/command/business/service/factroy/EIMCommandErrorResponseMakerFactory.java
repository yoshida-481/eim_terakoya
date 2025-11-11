package eim.command.business.service.factroy;

import eim.command.business.service.response.impl.EIMCommandErrorResponseMaker;
import eim.command.common.util.EIMCommandConstant;

public class EIMCommandErrorResponseMakerFactory {
	/**
	 * コマンドごとのコマンド実行実装インスタンスを生成する。
	 * @param cmdStr
	 * @return
	 * @throws Exception
	 */
	public EIMCommandErrorResponseMaker create(String cmdStr) throws Exception{
		EIMCommandErrorResponseMaker errorResponseMaker = null;
		
		if (cmdStr.equals(EIMCommandConstant.GET)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_BY_ID)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.PUT)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.LS)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.UPDATE_ATTR)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.FIND)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_ATTR)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_IN_BY_USER)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_OUT_BY_USER)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.ENC)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.VERIFY)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.LOGIN)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.SELECT)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.DELETE)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.CREATE)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.MKDIR)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.RM)){
			errorResponseMaker = new EIMCommandErrorResponseMaker();
		}
		else {
			throw new Exception();
		}
		
		
		return errorResponseMaker;
	}
}
