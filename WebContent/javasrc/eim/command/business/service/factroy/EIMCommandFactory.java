package eim.command.business.service.factroy;


import eim.bo.EIMResource;
import eim.command.business.service.EIMCommand;
import eim.command.business.service.response.impl.EIMCommandErrorResponseMaker;
import eim.command.business.service.result.EIMCommandResult;
import eim.command.common.util.EIMCommandConstant;

/**
 * コマンドfactory
 *
 *
 */
public class EIMCommandFactory {

	/**
	 * コマンドごとのコマンド実行インスタンス、レスポンス返却インスタンス、実行結果格納インスタンス
	 * を包括したインスタンスを生成する。
	 * @param cmdStr
	 * @return
	 * @throws Exception
	 */
	public EIMCommand create(String cmdStr) throws Exception{

		EIMCommand eimCommand = new EIMCommand();
		
		// cmdが取れていなければエラー
		if (cmdStr == null || cmdStr.equals("")) {
			eimCommand.setResultData(new EIMCommandResult());
			Object[] args = {EIMCommandConstant.CMD};
			String message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.NO.ESSENTIAL.PARAMETER", args);
			eimCommand.getResultData().setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), 
					EIMResource.getMessageValue("EIM.ERROR.CODE.NO.ESSENTIAL.PARAMETER"), message);
			eimCommand.setErrorResponseMaker(new EIMCommandErrorResponseMaker());
			
			return eimCommand;
		}
		
		cmdStr = cmdStr.toLowerCase();
		eimCommand.setCmdStr(cmdStr);
		EIMCommandExecuterFactory executerFactory = new EIMCommandExecuterFactory();
		EIMCommandResponseMakerFactory responseMakerFactory = new EIMCommandResponseMakerFactory();
		EIMCommandErrorResponseMakerFactory errorResponseMakerFactory = new EIMCommandErrorResponseMakerFactory();
		EIMCommandResultFactory resultFactory = new EIMCommandResultFactory();

		try {
			eimCommand.setCommandExecuter(executerFactory.create(cmdStr));
			eimCommand.setResponseMaker(responseMakerFactory.create(cmdStr));
			eimCommand.setErrorResponseMaker(errorResponseMakerFactory.create(cmdStr));
			eimCommand.setResultData(resultFactory.create(cmdStr));
		}catch (Exception e) {
			eimCommand.setResultData(new EIMCommandResult());
			eimCommand.getResultData().setTypeCodeMessage(EIMResource.getMessageValue("EIM.RESULT.TYPE.ERROR"), EIMResource.getMessageValue("EIM.ERROR.CODE.COMMAND.NO.EXIST"),
					EIMResource.getMessageValue("EIM.ERROR.LOGIC.INVALID.COMMAND"));
			eimCommand.setErrorResponseMaker(new EIMCommandErrorResponseMaker());
		}


		return eimCommand;
	}

}
