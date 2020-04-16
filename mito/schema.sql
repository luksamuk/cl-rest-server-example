CREATE TABLE "user" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "name" VARCHAR(80) NOT NULL,
    "birthdate" TIMESTAMPTZ NOT NULL,
    "address" VARCHAR(255) NOT NULL,
    "mail" VARCHAR(64) NOT NULL,
    "pass" VARCHAR(64) NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
CREATE UNIQUE INDEX "unique_user_mail" ON "user" ("mail");

CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY,
    "applied_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
