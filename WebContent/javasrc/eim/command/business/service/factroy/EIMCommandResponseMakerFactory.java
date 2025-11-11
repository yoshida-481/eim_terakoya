package eim.command.business.service.factroy;

import eim.command.business.service.response.EIMCommandResponseMaker;
import eim.command.business.service.response.impl.EIMCommandCheckinByUserResponseMaker;
import eim.command.business.service.response.impl.EIMCommandDefaultResponseMaker;
import eim.command.business.service.response.impl.EIMCommandFindResponseMaker;
import eim.command.business.service.response.impl.EIMCommandGetAttrResponseMaker;
import eim.command.business.service.response.impl.EIMCommandGetResponseMaker;
import eim.command.business.service.response.impl.EIMCommandLsResponseMaker;
import eim.command.business.service.response.impl.EIMCommandMkdirResponseMaker;
import eim.command.business.service.response.impl.EIMCommandPutResponseMaker;
import eim.command.business.service.response.impl.EIMCommandSelectResponseMaker;
import eim.command.business.service.response.impl.EIMCommandUpdateAttrResponseMaker;
import eim.command.common.util.EIMCommandConstant;

/**
 * レスポンス返却クラスfactory
 *
 *
 */
public class EIMCommandResponseMakerFactory {

	/**
	 * コマンドごとのレスポンス返却インスタンスを生成する。
	 * @param cmdStr
	 * @return
	 * @throws Exception
	 */
	public EIMCommandResponseMaker create(String cmdStr) throws Exception{
		EIMCommandResponseMaker responseMaker = null;

		if (cmdStr.equals(EIMCommandConstant.GET)){
			responseMaker = new EIMCommandGetResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_BY_ID)){
			responseMaker = new EIMCommandGetResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.PUT)){
			responseMaker = new EIMCommandPutResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.LS)){
			responseMaker = new EIMCommandLsResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.UPDATE_ATTR)){
			responseMaker = new EIMCommandUpdateAttrResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.FIND)){
			responseMaker = new EIMCommandFindResponseMaker();;
		}
		else if (cmdStr.equals(EIMCommandConstant.GET_ATTR)){
			responseMaker = new EIMCommandGetAttrResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_IN_BY_USER)){
			responseMaker = new EIMCommandCheckinByUserResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.CHECK_OUT_BY_USER)){
			responseMaker = new EIMCommandGetResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.ENC)){
			responseMaker = new EIMCommandDefaultResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.VERIFY)){
			responseMaker = new EIMCommandDefaultResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.LOGIN)){
			responseMaker = new EIMCommandDefaultResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.SELECT)){
			responseMaker = new EIMCommandSelectResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.DELETE)){
			responseMaker = new EIMCommandDefaultResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.CREATE)){
			responseMaker = new EIMCommandDefaultResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.MKDIR)){
			responseMaker = new EIMCommandMkdirResponseMaker();
		}
		else if (cmdStr.equals(EIMCommandConstant.RM)){
			responseMaker = new EIMCommandDefaultResponseMaker();
		}
		else {
			throw new Exception();
		}

		return responseMaker;
	}

}
