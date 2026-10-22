-- The MessageStore tables as nsi-safnari's evolutions leave them (conf/evolutions/default).
DROP TABLE IF EXISTS messages;
DROP TABLE IF EXISTS connections;
DROP SEQUENCE IF EXISTS pk_sequence;

CREATE SEQUENCE pk_sequence;

CREATE TABLE connections (
  id BIGINT DEFAULT NEXTVAL('pk_sequence') NOT NULL,
  connection_id TEXT NOT NULL,
  requester_nsa TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  deleted_at TIMESTAMP WITH TIME ZONE,
  correlation_id_generator_version SMALLINT,
  PRIMARY KEY (id)
);
CREATE UNIQUE INDEX connections_aggregated_connection_id ON connections (connection_id);

CREATE TABLE messages (
  id BIGINT DEFAULT NEXTVAL('pk_sequence') NOT NULL,
  connection_id BIGINT NOT NULL REFERENCES connections (id),
  correlation_id UUID,
  type TEXT NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  inbound_message_id BIGINT REFERENCES messages (id),
  PRIMARY KEY (id)
);
CREATE INDEX messages_connection_id_idx ON messages (connection_id, id);
CREATE INDEX messages_correlation_id ON messages (correlation_id);
CREATE INDEX messages_inbound_message_id_idx ON messages (inbound_message_id);
