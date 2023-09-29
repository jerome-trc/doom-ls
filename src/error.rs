use crossbeam::channel::SendError;
use lsp_server::{ErrorCode, Message, ProtocolError, RequestId, Response, ResponseError};

use crate::ErrorBox;

#[derive(Debug)]
pub(crate) enum Error {
	Response(Response),
	/// Failures to send messages over the LSP connection are given a separate
	/// variant to tell top-level code not to use the channel to report the error.
	Send(SendError<Message>),
	Process {
		source: Option<ErrorBox>,
		ctx: String,
	},
}

impl Error {
	#[must_use]
	pub(crate) fn map_to_response(self, req_id: RequestId, code: ErrorCode) -> Self {
		if matches!(self, Self::Response(_)) {
			return self;
		}

		let message = self.to_string();

		Self::Response(Response {
			id: req_id,
			result: None,
			error: Some(ResponseError {
				code: code as i32,
				message,
				data: None,
			}),
		})
	}
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Send(err) => write!(f, "failed to send a message: {err}"),
			Self::Process { source, ctx } => match source {
				Some(s) => {
					write!(f, "{ctx}: {s}")
				}
				None => {
					write!(f, "{ctx}")
				}
			},
			Self::Response(resp) => {
				write!(f, "failed to respond to a request: {resp:#?}")
			}
		}
	}
}

impl From<SendError<Message>> for Error {
	fn from(value: SendError<Message>) -> Self {
		Self::Send(value)
	}
}

impl From<ProtocolError> for Error {
	fn from(value: ProtocolError) -> Self {
		Self::Process {
			source: Some(Box::new(value)),
			ctx: "Language Server Protocol error".to_string(),
		}
	}
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Self::Process {
			source: Some(Box::new(value)),
			ctx: "file I/O failure".to_string(),
		}
	}
}
